
package org.tubs.table

import scala.collection.mutable.{TreeMap, ArrayBuffer}
import scala.math.Ordering
import java.io.OutputStream
import upickle.default.*

import org.tubs.executor.{CellContent, ExprValue, FormulaParser}
import org.tubs.utils.{Color, Todo}

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

  /**
   * Sets formula under particular cell to the new one
   */
  def set(key: CellKey, formula: String): Cell = setContent(key, formula, () => CellContent(key, formula))

  /**
   * Same as setFormula but also sets a value. 
   * Warning: should be used only in serialization
   * or deserialization to avoid formula-value mismatch.
   */
  def set(key: CellKey, formula: String, value: ExprValue): Cell = setContent(key, formula, () => CellContent(key, formula, value))

  /**
   * Gets a cell under specified key
   */
  def getCell(key: CellKey): Option[Cell] = cells.get(key)

  /**
   * Retrieves a value under specified key.
   * If there is no cell under such key, returns
   * empty VString.
   */
  def get(key: CellKey): ExprValue =
    getCell(key) match
      case Some(res) => res.content.value(using this)
      case None      => ExprValue.VString("")
        

  /**
   * Simply converts the table into human-readable json format
   */
  override def toString: String = write(this, 2)

  /**
   * Helper for setters. 
   * Parameter newContent is called only when there
   * is now cell under specified key and the new one
   * must be constructed.
   */
  private inline def setContent(key: CellKey, formula: String, inline newContent: () => CellContent): Cell = cells.updateWith(key) {
      case Some(cell) => 
        cell.content.refresh(formula)(using this)
        Some(cell)
      case None => 
        val res = Cell(newContent())
        cells += key -> res
        Some(res)
    }.get


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
case class CellKey(colId: Int, rowId: Int) derives ReadWriter:
  /**
   * Show to the user as <alphaCol><rowInt> - typical excel style
   */
  override def toString: String =
    val sb = new StringBuilder()
    var c = colId
    while (c > 0) {
      val remainder = (c - 1) % 26
      sb.append(('A' + remainder).toChar)
      c = (c - 1) / 26
    }
    sb.reverseInPlace().append(rowId).toString

object CellKey:
  /**
   * Define lexigraphical ordering
   */
  given Ordering[CellKey] with
    def compare(x: CellKey, y: CellKey) =
      val a = Ordering.Int.compare(x.colId, y.colId)
      if a != 0
        then a
        else Ordering.Int.compare(x.rowId, y.rowId)

  /**
   * Implicit conversion from tuple for conciseness
   */
  given Conversion[(Int, Int), CellKey] = (c, r) => CellKey(c, r)

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

