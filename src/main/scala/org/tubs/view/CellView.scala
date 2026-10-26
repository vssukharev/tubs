
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

