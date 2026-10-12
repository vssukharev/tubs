
package org.tubs.table

import scala.collection.mutable.{TreeMap, ArrayBuffer}
import scala.math.Ordering

import org.tubs.executor.CellContent
import org.tubs.utils.Color

/**
 * Table contains all semantic information about 
 * loaded from memory table, including row, column
 * and cell properties.
 */
class Table:
  var cells = TreeMap[CellKey, Cell]()
  var rows = ArrayBuffer[Row]()
  var cols = ArrayBuffer[Column]()

  def get(key: CellKey): Option[Cell] = cells.get(key)

  override def toString = cells.toString
  

/**
 * CellKey in combination with Cell fully represent
 * a cell with all mutable data needed.
 */
case class CellKey(colId: Int, rowId: Int)

case class Cell(content: CellContent, appearance: CellAppearance = CellAppearance()):
  override def toString = content.toString

object Cell:
  def apply(key: CellKey, formula: String): Cell = Cell(CellContent(key, formula))

/**
 * Properties of cell, impacting only the appearance
 */
case class CellAppearance(fgColor: Color = Color(), bgColor: Color = Color())

/**
 * Row and Column both contain all mutable properties
 * for particular row or column.
 */
case class Row(var width: Double)
case class Column(var width: Double)
  
/**
 * Ordering for the keys implemented lexigraphically 
 * in order to use in TreeMap
 */
object CellKey: 
  given Ordering[CellKey] with
    def compare(x: CellKey, y: CellKey) =
      val a = Ordering.Int.compare(x.colId, y.colId)
      if a != 0
        then a
        else Ordering.Int.compare(x.rowId, y.rowId)
  

