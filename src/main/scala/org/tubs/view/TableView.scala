
package org.tubs.view

import scala.collection.mutable.{ArrayDeque, ArrayBuffer}

import org.tubs.table.Table

// class TableView(val table: Table):
//   private type Row = (begin: Int, content: ArrayDeque[CellNode])
//
//   val rowHeaders: ArrayBuffer[HeaderNode]
//   val colHeaders: ArrayBuffer[HeaderNode]
//   val cells: ArrayBuffer[Row]
//  
// class CellNode(val key: CellKey):
//   var isEditing = false
//
//   val label = new Label:
//     alignment = Pos.CenterLeft
//     padding = Insets(2, 4, 2, 4)
//     maxWidth = Double.MaxValue
//     maxHeight = Double.MaxValue
//     style = "-fx-font-size: 12px;"
//
//   val container = new StackPane:
//     prefWidth = Defaults.CellWidth
//     prefHeight = Defaults.CellHeight
//     minWidth = Defaults.MinCellWidth
//     minHeight = Defaults.MinCellHeight
//     alignment = Pos.CenterLeft
//     children = Seq(label)
//
//
//   /**
//    * Updates border, background, and text colors according to cell appearance and selection state.
//    */
//   def updateAppearance(): Unit =
//     val table = currentTable.value
//     val app = table.getAppearance(key)
//     val hexFg = View.toHex(app.fgColor)
//     val hexBg = View.toHex(app.bgColor)
//     val isSel = selectedKey.value == key
//
//     val borderWidth = if isSel then "2.5px" else "0.5px"
//     val borderColor = if isSel then Defaults.SelectedBorderColor else Defaults.NormalBorderColor
//
//     container.style = s"-fx-background-color: $hexBg; -fx-border-color: $borderColor; -fx-border-width: $borderWidth;"
//     label.style = s"-fx-text-fill: $hexFg; -fx-font-size: 12px;"
//
//   /**
//    * Re-renders the displayed text from the evaluated cell value.
//    */
//   def refreshContent(): Unit =
//     val table = currentTable.value
//     val (value, _) = table.getValue(key)
//     val text = value match
//       case ExprValue.VString(str)   => str
//       case ExprValue.VInteger(v)    => v.toString
//       case ExprValue.VFloating(v)   => if v.isWhole then v.toLong.toString else f"$v%.4f".replaceAll("0+$", "").replaceAll("\\.$", "")
//       case ExprValue.VBoolean(b)    => b.toString
//       case ExprValue.VRange(l, r)   => s"$l..$r"
//       case ExprValue.VError         => "#error"
//     label.text = text
//
//   /**
//    * Begins in-cell editing mode.
//    */
//   def startEdit(): Unit =
//     if !isEditing then
//       isEditing = true
//       val table = currentTable.value
//       val formula = table.getFormula(key)
//       editor.text = formula
//       label.visible = false
//       editor.visible = true
//       editor.prefRowCount = if formula.contains("\n") then 3 else 1
//       editor.requestFocus()
//
//   /**
//    * Commits in-cell edit and updates the formula.
//    */
//   def commitEdit(): Unit =
//     if isEditing then
//       isEditing = false
//       val newFormula = editor.text.value
//       label.visible = true
//       editor.visible = false
//       setCellFormula(key, newFormula)
//


