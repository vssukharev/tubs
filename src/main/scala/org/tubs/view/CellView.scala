
package org.tubs.view

import scala.collection.mutable.{ArrayDeque, ArrayBuffer}
import scalafx.scene.control.Label
import scalafx.scene.layout.StackPane
import scalafx.geometry.{Insets, Pos}

import org.tubs.executor.ExprValue
import org.tubs.table.CellKey

class CellView(val key: CellKey):
  val label = new Label:
    alignment = Pos.CenterLeft
    padding = Insets(2, 4, 2, 4)
    maxWidth = Double.MaxValue
    maxHeight = Double.MaxValue
    style = "-fx-font-size: 12px;"

  val container = new StackPane:
    prefWidth = Defaults.CellWidth
    prefHeight = Defaults.CellHeight
    minWidth = Defaults.MinCellWidth
    minHeight = Defaults.MinCellHeight
    alignment = Pos.CenterLeft
    children = Seq(label)

  /**
   * Updates the whole state of a CellView
   */
  def update(): Unit =
    updateAppearance()
    refreshContent()

  /**
   * Ensures the given cell key is scrolled into view within the ScrollPane.
   * TODO: adjust it a bit
   */
  def makeVisible(): Unit =
    val targetY = (key.rowId - 1) * Defaults.CellHeight
    val targetX = (key.colId - 1) * Defaults.CellWidth
    val vpHeight = Sections.scrollPane.viewportBounds.value.getHeight
    val vpWidth = Sections.scrollPane.viewportBounds.value.getWidth
    val contentH = View.tableView.renderedRows * Defaults.CellHeight
    val contentW = View.tableView.renderedCols * Defaults.CellWidth

    if contentH > vpHeight && vpHeight > 0 then
      val curY = Sections.scrollPane.vvalue.value * (contentH - vpHeight)
      if targetY < curY then
        Sections.scrollPane.vvalue = (targetY / (contentH - vpHeight)).max(0.0).min(1.0)
      else if targetY + Defaults.CellHeight > curY + vpHeight then
        Sections.scrollPane.vvalue = ((targetY + Defaults.CellHeight - vpHeight) / (contentH - vpHeight)).max(0.0).min(1.0)

    if contentW > vpWidth && vpWidth > 0 then
      val curX = Sections.scrollPane.hvalue.value * (contentW - vpWidth)
      if targetX < curX then
        Sections.scrollPane.hvalue = (targetX / (contentW - vpWidth)).max(0.0).min(1.0)
      else if targetX + Defaults.CellWidth > curX + vpWidth then
        Sections.scrollPane.hvalue = ((targetX + Defaults.CellWidth - vpWidth) / (contentW - vpWidth)).max(0.0).min(1.0)

  /**
   * Updates border, background, and text colors according to cell appearance and selection state.
   */
  def updateAppearance(): Unit =
    val table = View.currentTable.value
    val app = table.getAppearance(key)
    val hexFg = View.toHex(app.fgColor)
    val hexBg = View.toHex(app.bgColor)
    val isSel = View.selectedKey.value == key

    val borderWidth = if isSel then "2.5px" else "0.5px"
    val borderColor = if isSel then Defaults.SelectedBorderColor else Defaults.NormalBorderColor

    container.style = s"-fx-background-color: $hexBg; -fx-border-color: $borderColor; -fx-border-width: $borderWidth;"
    label.style = s"-fx-text-fill: $hexFg; -fx-font-size: 12px;"

  /**
   * Re-renders the displayed text from the evaluated cell value.
   * Does not refreshes dependent cells. 
   */
  def refreshContent(): Unit =
    val table = View.currentTable.value
    val (value, _) = table.getValue(key)
    val text = value match
      case ExprValue.VString(str) => str
      case ExprValue.VInteger(v)  => v.toString
      case ExprValue.VFloating(v) => if v.isWhole then v.toLong.toString else f"$v%.4f".replaceAll("0+$", "").replaceAll("\\.$", "")
      case ExprValue.VBoolean(b)  => b.toString
      case ExprValue.VRange(l, r) => s"$l..$r"
      case ExprValue.VError       => "#error"
    label.text = text

  container.onMouseClicked = _ => View.selectedKey.value = key

