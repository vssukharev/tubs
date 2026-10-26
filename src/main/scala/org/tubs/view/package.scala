
package org.tubs.view

import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.*
import scalafx.scene.layout.*
import scalafx.scene.paint.Color
import scalafx.scene.effect.DropShadow
import scalafx.scene.transform.Scale
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.stage.FileChooser
import scalafx.beans.property.{ObjectProperty, StringProperty, IntegerProperty, DoubleProperty}

import org.tubs.table.*
import org.tubs.executor.*
import org.tubs.loader

/**
 * Displays an informational pop-up dialog.
 */
def info(message: String): Unit = showPopup(Alert.AlertType.Information, "Information", message)

/**
 * Displays a warning pop-up dialog.
 */
def warn(message: String): Unit = showPopup(Alert.AlertType.Warning, "Warning", message)

/**
 * Displays an error pop-up dialog.
 */
def error(message: String): Unit = showPopup(Alert.AlertType.Error, "Error", message)

/**
 * Helper to display a JavaFX Alert pop-up dialog on the UI thread.
 */
private def showPopup(alertType: Alert.AlertType, titleText: String, message: String): Unit =
  Platform.runLater {
    val alert = new Alert(alertType):
      title = titleText
      headerText = None
      contentText = message
    alert.showAndWait()
  }

/**
 * The main View object representing the spreadsheet GUI scene.
 */
object View:
  import Sections.*

  // --- Reactive UI state ---
  val currentTable = ObjectProperty[Table](Table())
  // val tableView = ObjectProperty[TableView](currentTable)
  val currentFileName = StringProperty("File name")
  val selectedKey = ObjectProperty[CellKey](CellKey(1, 1))

  val zoomScale = DoubleProperty(1.0)

  var renderedCols: Int = Defaults.DefaultCols
  var renderedRows: Int = Defaults.DefaultRows

  private val colHeaderLabels = mutable.ArrayBuffer[Label]()
  private val rowHeaderLabels = mutable.ArrayBuffer[Label]()
  private val cellNodes = mutable.Map[CellKey, CellNode]()

  /**
   * Represents an interactive cell in the GridPane, wrapping both a display Label and an in-cell TextArea.
   */
  class CellNode(val key: CellKey):
    var isEditing = false

    val label = new Label:
      alignment = Pos.CenterLeft
      padding = Insets(2, 4, 2, 4)
      maxWidth = Double.MaxValue
      maxHeight = Double.MaxValue
      style = "-fx-font-size: 12px;"

    val editor = new TextArea:
      prefRowCount = 1
      wrapText = true
      visible = false
      style = "-fx-font-size: 12px; -fx-font-family: 'monospace';"

    val container = new StackPane:
      prefWidth = Defaults.CellWidth
      prefHeight = Defaults.CellHeight
      minWidth = Defaults.CellWidth
      minHeight = Defaults.CellHeight
      alignment = Pos.CenterLeft
      children = Seq(label, editor)

    // Handle Enter and Shift+Enter in in-cell editor
    editor.filterEvent(KeyEvent.KeyPressed) { (e: KeyEvent) =>
      if e.code == KeyCode.Enter then
        if e.shiftDown then
          // Shift + Enter: insert newline at caret
          val pos = editor.caretPosition.value
          val txt = editor.text.value
          editor.text = txt.substring(0, pos) + "\n" + txt.substring(pos)
          editor.positionCaret(pos + 1)
          e.consume()
        else
          // Plain Enter: commit edit and switch to bottom adjacent cell
          e.consume()
          commitEdit()
          navigateTo(key.colId, key.rowId + 1)
    }

    /**
     * Updates border, background, and text colors according to cell appearance and selection state.
     */
    def updateAppearance(): Unit =
      val table = currentTable.value
      val app = table.getAppearance(key)
      val hexFg = toHex(app.fgColor)
      val hexBg = toHex(app.bgColor)
      val isSel = selectedKey.value == key

      val borderWidth = if isSel then "2.5px" else "0.5px"
      val borderColor = if isSel then Defaults.SelectedBorderColor else Defaults.NormalBorderColor

      container.style = s"-fx-background-color: $hexBg; -fx-border-color: $borderColor; -fx-border-width: $borderWidth;"
      label.style = s"-fx-text-fill: $hexFg; -fx-font-size: 12px;"

    /**
     * Re-renders the displayed text from the evaluated cell value.
     */
    def refreshContent(): Unit =
      val table = currentTable.value
      val (value, _) = table.getValue(key)
      val text = value match
        case ExprValue.VString(str)   => str
        case ExprValue.VInteger(v)    => v.toString
        case ExprValue.VFloating(v)   => if v.isWhole then v.toLong.toString else f"$v%.4f".replaceAll("0+$", "").replaceAll("\\.$", "")
        case ExprValue.VBoolean(b)    => b.toString
        case ExprValue.VRange(l, r)   => s"$l..$r"
        case ExprValue.VError         => "#error"
      label.text = text

    /**
     * Begins in-cell editing mode.
     */
    def startEdit(): Unit =
      if !isEditing then
        isEditing = true
        val table = currentTable.value
        val formula = table.getFormula(key)
        editor.text = formula
        label.visible = false
        editor.visible = true
        editor.prefRowCount = if formula.contains("\n") then 3 else 1
        editor.requestFocus()

    /**
     * Commits in-cell edit and updates the formula.
     */
    def commitEdit(): Unit =
      if isEditing then
        isEditing = false
        val newFormula = editor.text.value
        label.visible = true
        editor.visible = false
        setCellFormula(key, newFormula)

  /**
   * Converts 1-based column ID into letter representations (1 -> A, 26 -> Z, 27 -> AA).
   */
  def colIndexToLetters(colId: Int): String =
    val sb = new StringBuilder()
    var c = colId
    while c > 0 do
      val rem = (c - 1) % 26
      sb.append(('A' + rem).toChar)
      c = (c - 1) / 26
    sb.reverseInPlace().toString

  /**
   * Finds a CellNode for a given key, searching cellNodes map or grid containers.
   */
  def findCellNode(key: CellKey): Option[CellNode] = cellNodes.get(key)

  /**
   * Builds the initial grid of cells and headers.
   */
  def buildGrid(numCols: Int = Defaults.DefaultCols, numRows: Int = Defaults.DefaultRows): Unit =
    grid.children.clear()
    colHeaderLabels.clear()
    rowHeaderLabels.clear()
    cellNodes.clear()
    renderedCols = numCols
    renderedRows = numRows

    // Top-left corner cell
    val corner = new Label:
      prefWidth = Defaults.RowHeaderWidth
      prefHeight = Defaults.ColHeaderHeight
      minWidth = Defaults.RowHeaderWidth
      minHeight = Defaults.ColHeaderHeight
      alignment = Pos.Center
      style = s"-fx-background-color: ${Defaults.HeaderBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px;"
    grid.add(corner, 0, 0)

    // Index 0 padding for 1-based indexing in ArrayBuffer
    colHeaderLabels += corner
    rowHeaderLabels += corner

    // Column Headers
    for col <- 1 to numCols do
      val lbl = createColHeader(col)
      colHeaderLabels += lbl
      grid.add(lbl, col, 0)

    // Row Headers
    for row <- 1 to numRows do
      val lbl = createRowHeader(row)
      rowHeaderLabels += lbl
      grid.add(lbl, 0, row)

    // Cells: only store non-empty cells in cellNodes
    for row <- 1 to numRows; col <- 1 to numCols do
      val key = CellKey(col, row)
      val node = createCellNode(key)
      node.updateAppearance()
      node.refreshContent()
      if currentTable.value.getFormula(key).nonEmpty then
        cellNodes(key) = node
      grid.add(node.container, col, row)

    setHeaderActive(selectedKey.value)

  /**
   * Creates a column header label for given column ID.
   */
  private def createColHeader(col: Int): Label =
    val colLetter = colIndexToLetters(col)
    new Label(colLetter):
      prefWidth = Defaults.CellWidth
      prefHeight = Defaults.ColHeaderHeight
      minWidth = Defaults.CellWidth
      minHeight = Defaults.ColHeaderHeight
      alignment = Pos.Center
      style = s"-fx-background-color: ${Defaults.HeaderBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #495057;"

  /**
   * Creates a row header label for given row ID.
   */
  private def createRowHeader(row: Int): Label =
    new Label(row.toString):
      prefWidth = Defaults.RowHeaderWidth
      prefHeight = Defaults.CellHeight
      minWidth = Defaults.RowHeaderWidth
      minHeight = Defaults.CellHeight
      alignment = Pos.Center
      style = s"-fx-background-color: ${Defaults.HeaderBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #495057;"

  /**
   * Instantiates a CellNode and attaches mouse and text listeners.
   */
  private def createCellNode(key: CellKey): CellNode =
    val node = new CellNode(key)
    node.container.onMouseClicked = _ => selectCell(key)
    node.container.onMousePressed = e => if e.clickCount == 2 then node.startEdit()
    node.editor.focused.onChange { (_, _, isFocused) => if !isFocused then node.commitEdit() }
    node.editor.text.onChange { (_, _, newVal) => if node.isEditing then selectionContent.text = newVal }
    node

  /**
   * Dynamically expands the grid by adding new rows and precomputing values of formula cells.
   */
  def expandRows(count: Int = Defaults.PrelookupNum): Unit =
    val startRow = renderedRows + 1
    val endRow = renderedRows + count
    renderedRows = endRow

    for row <- startRow to endRow do
      val lbl = createRowHeader(row)
      rowHeaderLabels += lbl
      grid.add(lbl, 0, row)

      for col <- 1 to renderedCols do
        val key = CellKey(col, row)
        val node = createCellNode(key)
        if currentTable.value.getFormula(key).nonEmpty then
          currentTable.value.getValue(key) // Precompute value
          cellNodes(key) = node
        node.updateAppearance()
        node.refreshContent()
        grid.add(node.container, col, row)

  /**
   * Dynamically expands the grid by adding new columns and precomputing values of formula cells.
   */
  def expandCols(count: Int = Defaults.PrelookupNum): Unit =
    val startCol = renderedCols + 1
    val endCol = renderedCols + count
    renderedCols = endCol

    for col <- startCol to endCol do
      val lbl = createColHeader(col)
      colHeaderLabels += lbl
      grid.add(lbl, col, 0)

      for row <- 1 to renderedRows do
        val key = CellKey(col, row)
        val node = createCellNode(key)
        if currentTable.value.getFormula(key).nonEmpty then
          currentTable.value.getValue(key) // Precompute value
          cellNodes(key) = node
        node.updateAppearance()
        node.refreshContent()
        grid.add(node.container, col, row)

  /**
   * Checks current viewport scroll bounds and expands by PrelookupNum if nearing grid edges.
   */
  def checkAndExpand(): Unit =
    val vpHeight = scrollPane.viewportBounds.value.getHeight
    val vpWidth = scrollPane.viewportBounds.value.getWidth
    val contentHeight = renderedRows * Defaults.CellHeight
    val contentWidth = renderedCols * Defaults.CellWidth

    // Vertical check: expand rows if remaining unviewed rows <= PrelookupNum
    if contentHeight > vpHeight && vpHeight > 0 then
      val scrollY = scrollPane.vvalue.value * (contentHeight - vpHeight)
      val lastVisibleRow = ((scrollY + vpHeight) / Defaults.CellHeight).toInt
      if renderedRows - lastVisibleRow <= Defaults.PrelookupNum then
        expandRows(Defaults.PrelookupNum)

    // Horizontal check: expand cols if remaining unviewed cols <= PrelookupNum
    if contentWidth > vpWidth && vpWidth > 0 then
      val scrollX = scrollPane.hvalue.value * (contentWidth - vpWidth)
      val lastVisibleCol = ((scrollX + vpWidth) / Defaults.CellWidth).toInt
      if renderedCols - lastVisibleCol <= Defaults.PrelookupNum then
        expandCols(Defaults.PrelookupNum)

  /**
   * Navigates to given column and row, expanding grid and ensuring visibility.
   */
  def navigateTo(col: Int, row: Int): Unit =
    if row > renderedRows then expandRows((row - renderedRows) + Defaults.PrelookupNum)
    if col > renderedCols then expandCols((col - renderedCols) + Defaults.PrelookupNum)
    val newKey = CellKey(col, row)
    selectCell(newKey)
    ensureCellVisible(newKey)

  /**
   * Ensures the given cell key is scrolled into view within the ScrollPane.
   */
  def ensureCellVisible(key: CellKey): Unit =
    val targetY = (key.rowId - 1) * Defaults.CellHeight
    val targetX = (key.colId - 1) * Defaults.CellWidth
    val vpHeight = scrollPane.viewportBounds.value.getHeight
    val vpWidth = scrollPane.viewportBounds.value.getWidth
    val contentH = renderedRows * Defaults.CellHeight
    val contentW = renderedCols * Defaults.CellWidth

    if contentH > vpHeight && vpHeight > 0 then
      val curY = scrollPane.vvalue.value * (contentH - vpHeight)
      if targetY < curY then
        scrollPane.vvalue = (targetY / (contentH - vpHeight)).max(0.0).min(1.0)
      else if targetY + Defaults.CellHeight > curY + vpHeight then
        scrollPane.vvalue = ((targetY + Defaults.CellHeight - vpHeight) / (contentH - vpHeight)).max(0.0).min(1.0)

    if contentW > vpWidth && vpWidth > 0 then
      val curX = scrollPane.hvalue.value * (contentW - vpWidth)
      if targetX < curX then
        scrollPane.hvalue = (targetX / (contentW - vpWidth)).max(0.0).min(1.0)
      else if targetX + Defaults.CellWidth > curX + vpWidth then
        scrollPane.hvalue = ((targetX + Defaults.CellWidth - vpWidth) / (contentW - vpWidth)).max(0.0).min(1.0)

  /**
   * Sets the active selection to the specified cell key and updates header highlights and editors.
   */
  def selectCell(key: CellKey): Unit =
    val prevKey = selectedKey.value
    selectedKey.value = key

    findCellNode(prevKey).foreach(_.updateAppearance())
    findCellNode(key).foreach(_.updateAppearance())

    updateHeaderHighlight(prevKey, key)

    // Sync Section 4
    selectionIndex.text = key.toString
    val formula = currentTable.value.getFormula(key)
    if selectionContent.text.value != formula then
      selectionContent.text = formula

    // Sync Section 3
    val app = currentTable.value.getAppearance(key)
    fgPicker.value = app.fgColor
    bgPicker.value = app.bgColor

  /**
   * Updates formula in the table, re-evaluates, and selectively refreshes only the edited and dependent cells.
   * Empty cells are removed from cellNodes.
   */
  def setCellFormula(key: CellKey, formula: String): Unit =
    val table = currentTable.value
    table.set(key, formula)
    val (_, toChangeCells) = table.getValue(key)

    findCellNode(key).foreach { node =>
      if formula.trim.isEmpty then
        cellNodes.remove(key)
      else
        cellNodes(key) = node
      node.updateAppearance()
      node.refreshContent()
    }

    // Refresh ONLY the dependent cells
    for depKey <- toChangeCells do
      cellNodes.get(depKey).foreach { node =>
        node.updateAppearance()
        node.refreshContent()
      }


  /**
   * Sets active styling on row and column headers for given cell key
   */
  def setHeaderActive(key: CellKey): Unit =
    if key.colId < colHeaderLabels.size then
      colHeaderLabels(key.colId).style = s"-fx-background-color: ${Defaults.HeaderActiveBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #212529;"
    if key.rowId < rowHeaderLabels.size then
      rowHeaderLabels(key.rowId).style = s"-fx-background-color: ${Defaults.HeaderActiveBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #212529;"

  /**
   * Updates row and column header styles when selection changes
   */
  def updateHeaderHighlight(prev: CellKey, next: CellKey): Unit =
    if prev.colId != next.colId then
      if prev.colId < colHeaderLabels.size then
        colHeaderLabels(prev.colId).style = s"-fx-background-color: ${Defaults.HeaderBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #495057;"
      if next.colId < colHeaderLabels.size then
        colHeaderLabels(next.colId).style = s"-fx-background-color: ${Defaults.HeaderActiveBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #212529;"

    if prev.rowId != next.rowId then
      if prev.rowId < rowHeaderLabels.size then
        rowHeaderLabels(prev.rowId).style = s"-fx-background-color: ${Defaults.HeaderBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #495057;"
      if next.rowId < rowHeaderLabels.size then
        rowHeaderLabels(next.rowId).style = s"-fx-background-color: ${Defaults.HeaderActiveBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #212529;"

  /**
   * Converts a ScalaFX Color into a hex RGB string (#rrggbb).
   */
  private def toHex(c: Color): String =
    f"#${(c.red * 255).toInt}%02x${(c.green * 255).toInt}%02x${(c.blue * 255).toInt}%02x"

  /**
   * Attaches event handlers for toolbar buttons, keyboard navigation, color pickers, and zoom controls.
   */
  def setupEventHandlers(): Unit =
    // File manipulation buttons
    Buttons.open.onAction = _ =>
      val chooser = new FileChooser:
        title = "Open Table File"
      val file = chooser.showOpenDialog(null)
      if file != null then
        loader.load(file.getAbsolutePath) match
          case Success(t) =>
            currentTable.value = t
            currentFileName.value = file.getName
            buildGrid()
            selectCell(CellKey(1, 1))
          case Failure(err) =>
            error(s"Failed to open file: ${err.getMessage}")

    Buttons.save.onAction = _ =>
      loader.save(currentTable.value, currentFileName.value) match
        case Success(_) => info(s"Saved to ${currentFileName.value}")
        case Failure(err) => error(s"Failed to save file: ${err.getMessage}")

    Buttons.saveAsLocally.onAction = _ =>
      val chooser = new FileChooser:
        title = "Save Table As"
        initialFileName = currentFileName.value
      val file = chooser.showSaveDialog(null)
      if file != null then
        loader.save(currentTable.value, file.getAbsolutePath) match
          case Success(_) =>
            currentFileName.value = file.getName
            info(s"Saved as ${file.getAbsolutePath}")
          case Failure(err) =>
            error(s"Failed to save file: ${err.getMessage}")

    Buttons.saveAsRemotely.onAction = _ =>
      info("Remote saving functionality is coming soon.")

    // When the selection changed from content to something else
    selectionContent.focused.onChange { (_, _, isFocused) =>
      if !isFocused then setCellFormula(selectedKey.value, selectionContent.text.value)
    }

    // Enter / Shift+Enter handling in formula bar
    selectionContent.filterEvent(KeyEvent.KeyPressed) { (e: KeyEvent) =>
      if e.code == KeyCode.Enter then
        if e.shiftDown then
          val pos = selectionContent.caretPosition.value
          val txt = selectionContent.text.value
          selectionContent.text = txt.substring(0, pos) + "\n" + txt.substring(pos)
          selectionContent.positionCaret(pos + 1)
          e.consume()
        else
          e.consume()
          grid.requestFocus()
          val sel = selectedKey.value
          navigateTo(sel.colId, sel.rowId + 1)
    }

    // Color pickers change style of selected cell
    fgPicker.onAction = _ =>
      val key = selectedKey.value
      val table = currentTable.value
      val cell = table.getCell(key).getOrElse(table.set(key, ""))
      cell.appearance = cell.appearance.copy(fgColor = fgPicker.value.value)
      findCellNode(key).foreach(_.updateAppearance())

    bgPicker.onAction = _ =>
      val key = selectedKey.value
      val table = currentTable.value
      val cell = table.getCell(key).getOrElse(table.set(key, ""))
      cell.appearance = cell.appearance.copy(bgColor = bgPicker.value.value)
      findCellNode(key).foreach(_.updateAppearance())

    // Zoom controls
    val scaleTransform = new Scale(1.0, 1.0, 0, 0)
    grid.transforms = Seq(scaleTransform)

    Buttons.zoomOut.onAction = _ =>
      if zoomScale.value > Defaults.MinZoomScale then
        val factor = zoomScale.value - Defaults.ZoomStep
        zoomScale.value = factor
        scaleTransform.x = factor
        scaleTransform.y = factor

    Buttons.zoomIn.onAction = _ =>
      if zoomScale.value < Defaults.MaxZoomScale then
        val factor = zoomScale.value + Defaults.ZoomStep
        zoomScale.value = factor
        scaleTransform.x = factor
        scaleTransform.y = factor

    // Arrow keys navigate adjacent cells instead of scrolling
    scrollPane.addEventFilter(KeyEvent.KeyPressed, (e: KeyEvent) =>
      val sel = selectedKey.value
      e.code match
        case KeyCode.Up if sel.rowId > 1 =>
          e.consume()
          navigateTo(sel.colId, sel.rowId - 1)
        case KeyCode.Down =>
          e.consume()
          navigateTo(sel.colId, sel.rowId + 1)
        case KeyCode.Left if sel.colId > 1 =>
          e.consume()
          navigateTo(sel.colId - 1, sel.rowId)
        case KeyCode.Right =>
          e.consume()
          navigateTo(sel.colId + 1, sel.rowId)
        case _ =>
    )

    // Scroll listeners for dynamic expansion
    scrollPane.vvalue.onChange { (_, _, _) => checkAndExpand() }
    scrollPane.hvalue.onChange { (_, _, _) => checkAndExpand() }

    // Help Dialog
    Buttons.help.onAction = _ =>
      info(
        """tubs - Table Editor Help:
          |
          |Keyboard Controls:
          |  • Arrow Keys: Navigate adjacent cells
          |  • Double-Click: Edit cell in-place
          |  • Enter (during edit): Commit & move to bottom cell
          |  • Shift + Enter: Insert newline in cell / formula bar
          |
          |Formula Syntax (starts with '='):
          |  • Literals: 42, 3.14, true, false, "hello"
          |  • Arithmetic: +, -, *, /, mod, ^
          |  • Comparisons: ==, !=, <, >, <=, >=
          |  • Logical: and, or, not
          |  • Range: 1..10
          |  • Cell References: A1, B[i], [j]2, [i][j]
          |  • If Expression: if <cond> then <expr> else <expr>
          |  • For Loop: for <it>, <acc>=<init> in <range> do <body>
          |""".stripMargin
      )

  /**
   * Assembles and returns the main application Scene.
   */
  def createScene(table: Table = Table(), filename: String = "test.txt"): Scene =
    currentTable.value = table
    currentFileName.value = filename

    buildGrid()
    setupEventHandlers()
    selectCell(CellKey(1, 1))

    val topContainer = new VBox:
      spacing = 5
      padding = Insets(6)
      children = Seq(
        fileName,
        fileManip,
        styleEditor,
        curSelection
      )

    val scaleLabel = new Label("Scale 100%"):
      style = "-fx-font-size: 12px; -fx-text-fill: #495057;"

    zoomScale.onChange { (_, _, v) => 
      val p = (v.doubleValue * 100).toInt
      scaleLabel.text = s"Scale $p%" }

    val footer = new HBox:
      padding = Insets(4, 10, 4, 10)
      alignment = Pos.CenterRight
      spacing = 8
      style = s"-fx-background-color: ${Defaults.HeaderBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 1 0 0 0;"
      children = Seq(
        new Label("Scale") { style = "-fx-text-fill: #6c757d; -fx-font-size: 12px;" },
        Buttons.zoomOut,
        scaleLabel,
        Buttons.zoomIn
      )

    new Scene(980, 680):
      root = new StackPane:
        children = Seq(
          new BorderPane:
            top = topContainer
            center = scrollPane
            bottom = footer
        )

  /**
   * Scene getter for default state.
   */
  def mainScene: Scene = createScene(currentTable.value, currentFileName.value)

