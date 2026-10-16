
package org.tubs.table

import scala.collection.mutable.{TreeMap, ArrayBuffer}
import scala.math.Ordering
import java.io.OutputStream
import upickle.default.*

import org.tubs.executor.CellContent
import org.tubs.utils.Color

/**
 * Table contains all semantic information about 
 * loaded from memory table, including row, column
 * and cell properties.
 *
 * cells map is serialized like a regular array of Cells.
 * Because each Cell practically contains a key, we will
 * avoid key duplication.
 */
class Table:
  val cells = TreeMap[CellKey, Cell]()
  val rows = ArrayBuffer[Row]()
  val cols = ArrayBuffer[Column]()

  def get(key: CellKey): Option[Cell] = cells.get(key)

object Table:
  given ReadWriter[Table] = readwriter[ujson.Value].bimap(
    t => ujson.Obj(
      "cells" -> writeJs(t.cells.values),
      "rows"  -> writeJs(t.rows.toSeq),
      "cols"  -> writeJs(t.cols.toSeq)
    ),
    json =>
      val t = Table()
      read[Seq[Cell]](json("cells")).foreach(cell => t.cells += (cell.content.key -> cell))
      read[Seq[Row]](json("rows")).foreach(t.rows += _)
      read[Seq[Column]](json("cols")).foreach(t.cols += _)
      t
  )

/**
 * CellKey in combination with Cell fully represent
 * a cell with all mutable data needed.
 */
case class CellKey(colId: Int, rowId: Int)

object CellKey:
  /**
   * Serialize to <col>:<row>
   */
  given ReadWriter[CellKey] = readwriter[String].bimap(
    k => s"${k.colId}:${k.rowId}",
    s => 
      val Array(c, r) = s.split(":")
      CellKey(c.toInt, r.toInt)
   )

  /**
   * Define lexigraphical ordering
   */
  given Ordering[CellKey] with
    def compare(x: CellKey, y: CellKey) =
      val a = Ordering.Int.compare(x.colId, y.colId)
      if a != 0
        then a
        else Ordering.Int.compare(x.rowId, y.rowId)


case class Cell(content: CellContent, var appearance: CellAppearance = CellAppearance()) derives ReadWriter

/**
 * Properties of cell, impacting only the appearance
 */
case class CellAppearance(fgColor: Color = Color(), bgColor: Color = Color()) derives ReadWriter

/**
 * Row and Column both contain all mutable properties
 * for particular row or column.
 */
case class Row(var width: Double)    derives ReadWriter
case class Column(var width: Double) derives ReadWriter

