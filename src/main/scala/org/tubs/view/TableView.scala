
package org.tubs.view

import scala.collection.mutable.{ArrayDeque, ArrayBuffer}
import scalafx.scene.control.Label
import scalafx.scene.layout.GridPane
import scalafx.geometry.Pos

import org.tubs.table.{Table, CellKey}
import org.tubs.utils.Todo

class TableView(table: Table):
  /**
   * The rows are not fully rendered unless it is needed.
   * It is reached via Deque. And the column index of the first
   * element of the deque.
   */
  private type Row = (begin: Int, content: ArrayBuffer[CellView])

  val rowHeaders = ArrayBuffer[HeaderView]()
  val colHeaders = ArrayBuffer[HeaderView]()
  val cells = ArrayBuffer[Row]()

  /**
   * Init steps
   */
  createCells()
 
  /**
   * Number of rendered rows and columns.
   * Note that if row 'r' is rendered, it doesn't
   * mean that all cells under 'r' are rendered.
   * The same with columns.
   */
  def renderedRows = rowHeaders.size
  def renderedCols = colHeaders.size

  /**
   * Retrieves cell view and initializes it if it is not yet
   * rendered.
   */
  def getCellView(key: CellKey): CellView =
    if key.colId >= renderedCols then appendColumns(key.colId - renderedCols + Defaults.PrelookupNum)
    if key.rowId >= renderedRows then appendRows(key.rowId - renderedRows + Defaults.PrelookupNum)
    val (begin, rowBuf) = cells(key.rowId - 1)
    rowBuf(key.colId - 1 - begin)

  /**
   * Updates formula in the table, re-evaluates, and selectively refreshes 
   * only the edited and dependent cells.
   */
  def setCellFormula(key: CellKey, formula: String): Unit =
    table.set(key, formula)
    def traverseDeps(key: CellKey): Unit =
      val (_, dependent) = table.getValue(key)
      for depKey <- dependent do 
        this.getCellView(depKey).refreshContent()
        traverseDeps(depKey)
    traverseDeps(key)

  /**
   * Updates row and column header styles when selection changes
   */
  def updateHeaderHighlight(prev: CellKey, next: CellKey): Unit =
    if prev.colId != next.colId then
      colHeaders(prev.colId-1).label.style = s"-fx-background-color: ${Defaults.HeaderBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #495057;"
      colHeaders(next.colId-1).label.style = s"-fx-background-color: ${Defaults.HeaderActiveBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #212529;"

    if prev.rowId != next.rowId then
      rowHeaders(prev.rowId-1).label.style = s"-fx-background-color: ${Defaults.HeaderBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #495057;"
      rowHeaders(next.rowId-1).label.style = s"-fx-background-color: ${Defaults.HeaderActiveBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px; -fx-font-weight: bold; -fx-font-size: 11px; -fx-text-fill: #212529;"

  /**
   * Initializes cells as a square and immediately
   * adds them to a grid.
   */
  def createCells(): Unit = 
    // Top-left corner cell
    val corner = new Label:
      prefWidth = Defaults.CellWidth
      prefHeight = Defaults.CellHeight
      alignment = Pos.Center
      style = s"-fx-background-color: ${Defaults.HeaderBgColor}; -fx-border-color: ${Defaults.HeaderBorderColor}; -fx-border-width: 0.5px;"

    Sections.grid.add(corner, 0, 0)

    appendColumns(Defaults.DefaultCols)
    appendRows(Defaults.DefaultRows)
 
  /**
   * Append n rows and initializes the cells in there
   */
  def appendRows(n: Int): Unit =
    val startRow = renderedRows + 1
    val endRow = renderedRows + n

    cells.sizeHint(endRow)
    rowHeaders.sizeHint(endRow)

    for i <- startRow to endRow do
      val headerView = HeaderView(i.toString)
      Sections.grid.add(headerView.label, 0, i)
      rowHeaders += headerView

      // TODO: optimize for deque
      val rowBuf = new ArrayBuffer[CellView](renderedCols)
      for j <- 1 to renderedCols do 
        val key = CellKey(j, i)
        val cellView = CellView(key)
        Sections.grid.add(cellView.container, j, i)
        cellView.update()
        rowBuf += cellView

      cells += (0 -> rowBuf)
  
  /**
   * Appends n columns and initializes the cells
   */
  def appendColumns(n: Int): Unit =
    val startCol = renderedCols + 1
    val endCol = renderedCols + n

    colHeaders.sizeHint(endCol)

    for j <- startCol to endCol do
      val headerView = HeaderView(View.colIndexToLetters(j))
      Sections.grid.add(headerView.label, j, 0)
      colHeaders += headerView

    // TODO: optimize for deque
    for i <- 1 to renderedRows do 
      val (begin, rowBuf) = cells(i - 1)
      rowBuf.sizeHint(endCol - begin)

      for j <- startCol to endCol do
        val key = CellKey(j, i)
        val cellView = CellView(key)
        Sections.grid.add(cellView.container, j, i)
        cellView.update()
        rowBuf += cellView



