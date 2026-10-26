
package org.tubs.view

import scala.collection.mutable
import scala.util.{Try, Success, Failure}
import scalafx.Includes.*
import scalafx.application.Platform
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, Alert}
import scalafx.scene.layout.*
import scalafx.scene.paint.Color
import scalafx.scene.effect.DropShadow
import scalafx.scene.transform.Scale
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.stage.FileChooser
import scalafx.beans.property.{ObjectProperty, StringProperty, IntegerProperty, DoubleProperty}
import scalafx.beans.binding.Bindings
import scalafx.beans.binding.Bindings.*

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

  /**
   * This flag must be a guard from null dereferencing
   * of tableView or currentTable. Unfortunately, such
   * initial state is the most appropriately implemented
   * using null object - nothing to do here.
   */
  var initialized = false

  /**
   * Reative UI state
   */
  val selectedKey = ObjectProperty[CellKey](CellKey(1, 1))
  val currentFileName = StringProperty("File name")
  val zoomScale = DoubleProperty(1.0)
  val currentTable = ObjectProperty[Table](Table())
  var tableView: TableView = null

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
   * Converts a ScalaFX Color into a hex RGB string (#rrggbb).
   */
  def toHex(c: Color): String =
    f"#${(c.red * 255).toInt}%02x${(c.green * 255).toInt}%02x${(c.blue * 255).toInt}%02x"

  /**
   * Attaches event handlers for toolbar buttons, keyboard navigation, color pickers, and zoom controls.
   */
  def setupEventHandlers(): Unit =
    // File manipulation buttons
    // Buttons.open.onAction = _ =>
    //   val chooser = new FileChooser:
    //     title = "Open Table File"
    //   val file = chooser.showOpenDialog(null)
    //   if file != null then
    //     loader.load(file.getAbsolutePath) match
    //       case Success(t) =>
    //         currentTable.value = t
    //         currentFileName.value = file.getName
    //         buildGrid()
    //         selectCell(CellKey(1, 1))
    //       case Failure(err) =>
    //         error(s"Failed to open file: ${err.getMessage}")

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

    // View and current table sync
    currentTable.onChange { (_, _, _) => tableView = TableView(currentTable.value) }

    // Key selection synchronization
    selectedKey.onChange {
      (_, oldKey, newKey) =>
        val oldView = tableView.getCellView(oldKey)
        val newView = tableView.getCellView(newKey)
        oldView.update()
        newView.updateAppearance()

        tableView.updateHeaderHighlight(oldKey, newKey)

        // Sync formula editor section
        selectionIndex.text = newKey.toString
        val formula = currentTable.value.getFormula(newKey)
        if selectionContent.text.value != formula then
          selectionContent.text = formula

        // Sync cell properties section
        val app = currentTable.value.getAppearance(newKey)
        fgPicker.value = app.fgColor
        bgPicker.value = app.bgColor
    }

    // When the selection changed from content to something else
    selectionContent.focused.onChange { 
      (_, _, isFocused) =>
        if !isFocused then tableView.setCellFormula(selectedKey.value, selectionContent.text.value)
    }

    // Enter / Shift+Enter handling in formula bar
    selectionContent.filterEvent(KeyEvent.KeyPressed) { 
      (e: KeyEvent) =>
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
            selectedKey.value = CellKey(sel.colId, sel.rowId + 1)
    }

    // Color pickers change style of selected cell
    fgPicker.onAction = _ =>
      val key = selectedKey.value
      val table = currentTable.value
      val cell = table.getCell(key).getOrElse(table.set(key, ""))
      cell.appearance = cell.appearance.copy(fgColor = fgPicker.value.value)
      tableView.getCellView(key).updateAppearance()

    bgPicker.onAction = _ =>
      val key = selectedKey.value
      val table = currentTable.value
      val cell = table.getCell(key).getOrElse(table.set(key, ""))
      cell.appearance = cell.appearance.copy(bgColor = bgPicker.value.value)
      tableView.getCellView(key).updateAppearance()

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
          selectedKey.value = CellKey(sel.colId, sel.rowId - 1)
        case KeyCode.Down =>
          e.consume()
          selectedKey.value = CellKey(sel.colId, sel.rowId + 1)
        case KeyCode.Left if sel.colId > 1 =>
          e.consume()
          selectedKey.value = CellKey(sel.colId - 1, sel.rowId)
        case KeyCode.Right =>
          e.consume()
          selectedKey.value = CellKey(sel.colId + 1, sel.rowId)
        case _ =>
    )

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
  def createScene(table: Table, filename: String): Scene =
    setupEventHandlers()
    currentTable.value = table
    currentFileName.value = filename
    initialized = true

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


