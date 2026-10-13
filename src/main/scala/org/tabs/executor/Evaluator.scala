
package org.tubs.executor

import org.tubs.table.*
import org.tubs.view
import scala.collection.mutable.Map

/**
 * Organizes the table evaluation process, performing dynamic
 * type check via type checker and then calculating the result.
 * Also handles circular dependencies on cells.
 */
object Evaluator:
  import ExprValue.*

  /**
   * Extracts built AST, evaluates all given expressions 
   * and follows the referenced cells, recursively
   * calling itself.
   *
   * In case of error, returns ExprValue.VError, which leads
   * all dependent cells to also have ExprValue.VError.
   */
  def eval(content: CellContent)(using context: Table): ExprValue =
    VError

  def eval(prevKey: CellKey, content: CellContent)(using context: Table): ExprValue =
    content.addDependent(prevKey)
    eval(content)

  /**
   * Helper overloading for eval 
   */
  def eval(key: CellKey)(using context: Table): ExprValue = context.get(key) match
    case Some(cell) => eval(key, cell.content)
    case None       =>
      view.error(s"Cell $key is empty and can't be referenced")
      VError

