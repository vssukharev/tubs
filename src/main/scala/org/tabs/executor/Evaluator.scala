
package org.tubs.executor

import org.tubs.table.*
import org.tubs.view
import scala.collection.mutable.Map

/**
 * The collection of symbol names, which is considered to be
 * retrieved right after 
 */
opaque type CellSymbols = Map[String, ExprValue]

object CellSymbols:
  def apply(): CellSymbols = Map.empty[String, ExprValue]

  extension (cs: CellSymbols)
    def get(name: String): Option[ExprValue] = cs.get(name)
    def update(name: String, v: ExprValue): Unit = cs.put(name, v)
    def contains(name: String): Boolean = cs.contains(name)
/**
 * Organizes the table evaluation process, performing dynamic
 * type check via type checker and then calculating the result.
 * Also handles circular dependencies on cells.
 */
class Evaluator:
  import ExprValue.*

  /**
   * Extracts built AST, evaluates all given expressions 
   * and follows the referenced cells, recursively
   * calling itself.
   *
   * In case of error, returns ExprValue.VError, which leads
   * all dependent cells to also have ExprValue.VError.
   */
  def eval(prevKey: CellKey, curKey: CellKey, content: CellContent)(using context: Table): ExprValue =
    content.addDependent(prevKey)
    eval(curKey, content)

  def eval(key: CellKey, content: CellContent)(using context: Table): ExprValue =
    var symbols = CellSymbols()
    VError

  /**
   * Helper overloading for eval 
   */
  def eval(key: CellKey)(using context: Table): ExprValue = context.get(key) match
    case Some(cell) => eval(key, cell.content)
    case None       =>
      view.error(s"Cell $key is empty and can't be referenced")
      VError

