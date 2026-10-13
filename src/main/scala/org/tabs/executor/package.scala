
package org.tubs.executor

import upickle.default.*

import scala.collection.mutable.Set
import org.tubs.table.*
import org.tubs.utils.Todo
import org.tubs.view

/**
 * Basically the whole semantic content of a cell.
 *
 * Because CellContent is intended to be constructed
 * after Table deserialization, it is guaranteed that
 * the type is known at construction time. It actually
 * simplifies the logic as our is always synchronized
 * with the plain string representation (formula)
 *
 * During construction and string change it rebuilds
 * AST and resets the value. Computation of value is
 * lazy so that we are not doing it immediately after
 * type check.
 */
class CellContent(
  val key: CellKey,
  private var _formula: String, 
  private[executor] var _vopt: ExprValueOpt, 
):

  import ExprValue.*
  import ExprValueOpt.*

  private var _ast        = CellAST(key, _formula)
  private val _dependents = Set[CellKey]()
  private val _dependencies = Set[CellKey]()

  /**
   * Private members getters
   */
  def formula: String    = _formula
  def vopt: ExprValueOpt = _vopt
  def ast: CellAST       = _ast

  /**
   * Retrieves a value from this CellContent lazily and handles circular dependencies
   */
  def value(using evaluator: Evaluator, context: Table): ExprValue = _vopt match
    case Result(v) => v
    case Pending =>
      view.error(s"Circular dependency detected on cell $key")
      _vopt = Result(VError)
      VError
    case Empty =>
      _vopt = Pending
      val res = evaluator.eval(key, this)
      _vopt = Result(res)
      res

  def addDependent(depKey: CellKey): Unit =
    _dependents += depKey

  def removeDependent(depKey: CellKey): Unit =
    _dependents -= depKey

  /**
   * Set cell's value to empty, recursively resetting
   * all of the dependent cells.
   */
  def reset()(using context: Table): Unit =
    if _vopt != ExprValueOpt.Empty then
      _vopt = ExprValueOpt.Empty
      for depKey <- _dependents do
        context.get(depKey).foreach(_.content.reset())

  /**
   * Traverses dependencies and deletes instance of this cell 
   * in their _dependents
   */
  def unsubscribe()(using context: Table): Unit =
    for depKey <- _dependencies do
      context.get(depKey).foreach(_.content.removeDependent(key))
    _dependencies.clear()

  /**
   * Updates the formula and rebuilds AST
   */
  def refresh(newFormula: String)(using context: Table): Unit =
    unsubscribe()
    reset()
    _formula = newFormula
    _ast = CellAST(key, _formula)

/**
 * Companion object for CellContent with contructors
 */
object CellContent:
  def apply(key: CellKey, formula: String): CellContent =
    new CellContent(key, formula, ExprValueOpt.Empty)

  def apply(key: CellKey, formula: String, v: ExprValue): CellContent =
    val res = CellContent(key, formula)
    res._vopt = ExprValueOpt.Result(v)
    res

  /**
   * CellContent Representation in json: formula and optional value
   */
  given ReadWriter[CellContent] = readwriter[ujson.Value].bimap(
    obj => ujson.Obj(
      "key"     -> write(obj.key),
      "formula" -> obj.formula,
      "value"   -> write(obj.vopt.toOption)
    ),
    json => 
      val key = read[CellKey](json("key"))
      val formula = read[String](json("formula"))
      val value = read[Option[ExprValue]](json("value"))
      value match
        case Some(x) => CellContent(key, formula, x)
        case None    => CellContent(key, formula)
  )

/**
 * All semantic types of expressions and their values.
 * Each type is prefixed with T for disambiguation.
 * 
 * Boolean - true or false
 * Integer - 32-bit signed integer
 * Floating - 64-bit floating point number
 * Range - a pair, representing an integer continuous range
 * Error - plainly a signal of an error; error message is instantly
 *         revieled to a user via com.tubs.view
 */
enum ExprValue derives ReadWriter:
  case VBoolean(v: Boolean)  
  case VInteger(v: Int)      
  case VFloating(v: Double)  
  case VRange(l: Int, u: Int)
  case VString(str: String)
  case VError
  
/**
 * Cell traverse state, which is typically used for
 * Evaluator and TypeChecker in order to handle intermediate
 * errors, circular dependencies of cells, etc.
 */
enum ExprValueOpt:
  case Empty
  case Pending
  case Result(x: ExprValue)

  def toOption: Option[ExprValue] = this match
    case Empty | Pending => None
    case Result(x)       => Some(x)

object ExprValueOpt:
  def apply(opt: Option[ExprValue]): ExprValueOpt = opt match
    case None    => Empty
    case Some(x) => Result(x)

