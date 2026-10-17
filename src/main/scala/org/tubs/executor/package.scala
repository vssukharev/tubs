
package org.tubs.executor

import scala.compiletime
import scala.compiletime.erasedValue
import upickle.default.*

import scala.collection.mutable.{Set, Stack}
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

  private var _ast          = CellAST(key, _formula)
  private val _dependents   = Set[CellKey]()
  private val _dependencies = Set[CellKey]()
  private val _symbols      = Stack[(String, ExprValue)]()

  /**
   * Private members getters
   */
  def formula: String    = _formula
  def vopt: ExprValueOpt = _vopt
  def ast: CellAST       = _ast

  /**
   * Retrieves a value from this CellContent lazily and handles circular dependencies
   */
  def value(using context: Table): ExprValue = _vopt match
    case Result(v) => v
    case Pending =>
      view.error(s"Circular dependency detected on cell $key")
      _vopt = Result(VError)
      VError
    case Empty =>
      _vopt = Pending
      val res = Evaluator.eval(this)
      _vopt = Result(res)
      res

  /**
   * Updates the formula and rebuilds AST
   */
  def refresh(newFormula: String)(using context: Table): Unit =
    removeDependencies()
    resetValue()
    _formula = newFormula
    _ast = CellAST(key, _formula)

  /**
   * Traverses dependencies and deletes instance of this cell 
   * in their _dependents
   */
  def removeDependencies()(using context: Table): Unit =
    for depKey <- _dependencies do
      context.getCell(depKey).foreach(_.content.removeDependent(key))
    _dependencies.clear()

  /**
   * Set cell's value to empty, recursively resetting
   * all of the dependent cells.
   */
  def resetValue()(using context: Table): Unit =
    if _vopt != ExprValueOpt.Empty then
      _vopt = ExprValueOpt.Empty
      for depKey <- _dependents do
        context.getCell(depKey).foreach(_.content.resetValue())

  /**
   * Sends a message to a user mentioning the current cell 
   */
  def error(msg: String): ExprValue =
    view.error(s"In $key cell: " + msg)
    VError

  /**
   * Dependent cells manipulation methods
   */
  def addDependent(depKey: CellKey): Unit    = _dependents += depKey
  def removeDependent(depKey: CellKey): Unit = _dependents -= depKey

  /**
   * Variables stack manipulation methods
   */
  def pushSymbol(id: String, value: ExprValue): Unit = _symbols.push(id -> value)
  def popSymbol(): ExprValue                         = _symbols.pop()._2
  def getSymbolValue(id: String): Option[ExprValue]  = _symbols.findLast(_._1 == id).map(_._2)
  def changeSymbolValue(id: String, value: ExprValue): Unit = 
    val i = _symbols.lastIndexWhere(_._1 == id)
    _symbols.update(i, id -> value)

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
      "key"     -> writeJs(obj.key),
      "formula" -> obj.formula,
      "value"   -> writeJs(obj.vopt.toOption)
    ),
    json => 
      val key     = read[CellKey](json("key"))
      val formula = read[String](json("formula"))
      val value   = read[Option[ExprValue]](json("value"))
      value match
        case Some(x) => CellContent(key, formula, x)
        case None    => CellContent(key, formula)
  )

/**
 * All semantic types of expressions.
 * Each type is prefixed with T for disambiguation.
 * 
 * Boolean - true or false
 * Integer - 32-bit signed integer
 * Floating - 64-bit floating point number
 * Range - a pair, representing an integer continuous range
 * Error - plainly a signal of an error; error message is instantly
 *         revieled to a user via com.tubs.view
 */
enum ExprType:
  case TBoolean, TInteger, TFloating, TRange, TString, TError
  override def toString = this match
    case TBoolean => "Boolean"
    case TInteger => "Integer"
    case TFloating => "Floating"
    case TRange => "Range"
    case TString => "String"
    case TError => "ErrorType"

object ExprType:
  import ExprValue.*

  transparent inline def fromValue[T <: ExprValue] =
    inline erasedValue[T] match
      case _: VBoolean    => TBoolean
      case _: VInteger    => TInteger
      case _: VFloating   => TFloating
      case _: VRange      => TRange
      case _: VString     => TString
      case _: VError.type => TError
      case _: ExprValue   => compiletime.error("fromValue got ExprValue type")
      case _              => compiletime.error("fromValue has retrieved an unknown type")


/**
 * Values for the types above
 */
enum ExprValue derives ReadWriter:
  import ExprType.*

  case VBoolean(v: Boolean)  
  case VInteger(v: Int)      
  case VFloating(v: Double)  
  case VRange(l: Int, r: Int)
  case VString(v: String)
  case VError

  def exprType: ExprType = this match
    case VBoolean(_)  => fromValue[VBoolean]
    case VInteger(_)  => fromValue[VInteger]
    case VFloating(_) => fromValue[VFloating]
    case VRange(_, _) => fromValue[VRange]
    case VString(_)   => fromValue[VString]
    case VError       => fromValue[VError.type]
  
  /**
   * If VError, return VError, otherwise apply f
   */
  def map(f: ExprValue => ExprValue): ExprValue = this match
    case VError => VError
    case valid  => f(valid)

  /**
   * Same as map
   */
  def flatMap(f: ExprValue => ExprValue): ExprValue = map(f)

  /**
   * Invalid values are filtered as VError
   */
  def withFilter(p: ExprValue => Boolean): ExprValue = this match
    case VError => VError
    case valid if p(valid) => valid
    case _ => VError

  /**
   * Unboxes the value of this, converting it to the particular ExprValue.
   * Warning: must be used when being absolutely sure regarding the type.
   */
  inline def unbox[T <: ExprValue]: T = this.asInstanceOf[T]

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

