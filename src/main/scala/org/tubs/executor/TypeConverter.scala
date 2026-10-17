
package org.tubs.executor

import scala.compiletime.erasedValue

object TypeConverter:
  import ExprValue.*
  import ExprType.*
  import TypeConversion.*
    
  type ExprValueThunk = () => ExprValue
  type Converter = PartialFunction[ExprValue, ExprValue]

  /**
   * Lazily evaluated type conversion result,
   * storing a thunk instead of the real value.
   */
  enum TypeConversion:
    case Available(thunk: () => ExprValue)
    case Fail
    def value: ExprValue   = this match 
      case Available(thunk) => thunk()
      case Fail             => throw new NoSuchElementException("Cannot extract value from failed conversion")

  /**
   * Converts source value sourceV to target type targetT.
   * If conversion is successfull, lazily computes it's result.
   * Same types are also accepted and signalized with special
   * value Same.
   */
  def convert(sourceV: ExprValue, targetT: ExprType)(using content: CellContent): TypeConversion =
    val sourceT = sourceV.exprType
    if (sourceT == targetT) then 
      Available(() => sourceV)
    else
      conversionMap(content)
        .andThen(f => Available(() => f(sourceV)))
        .applyOrElse((sourceT, targetT), _ => Fail)
  /**
   * Converts and automatically unwraps expression values. Also can be called
   * with already wrapped value.
   */
  def convertUnwrap(sourceV: ExprValue, targetT: ExprType)(using content: CellContent): ExprValue =
    convert(sourceV, targetT) match 
      case Available(t) => t()
      case Fail         => content.error(s"Unable to convert from type ${sourceV.exprType} to ${targetT} during formula calculations.")

  /**
   * Statically convert sourceV to type T.
   */
  inline def convertStatic[T <: ExprValue](sourceV: ExprValue)(using content: CellContent): ExprValue =
    convertUnwrap(sourceV, ExprType.fromValue[T])

  /**
   * Compares two being the subtypes of each other.
   * Possible values:
   *  -1 - at is subtype of bt
   *   0 - there isn't any subtype relationship between at and bt
   *   1 - bt is subtype of at
   *
   * Subtyping is used when solving ambiguities in
   * egalitarian binary operations (equality, for instance).
   * If one type is a subtype of another, we have
   * a strong guarantee to convert one to another
   * and the corresponding conversion from table
   * is applied. If 
   * between at and bt, 0 is returned.
   *
   * Note that if each type is a subtype of another,
   * left type is considered to have bigger priority,
   * so the right is returned.
   * In general, both types must be equal in such scenario.
   */
  def subtypesCompare(at: ExprType, bt: ExprType)(using content: CellContent): Int =
    if      bt.isSubtypeOf(at) then 1
    else if at.isSubtypeOf(bt) then -1
    else 0

  /**
   * The logic of subtyping relationships.
   */
  extension (at: ExprType)
    def isSubtypeOf(bt: ExprType): Boolean = (at, bt) match
      case (TInteger, TFloating) => true
      case _                     => at == bt

  val conversionMap: CellContent => PartialFunction[(ExprType, ExprType), Converter] = content => {
    case (TInteger,  TFloating) => { case VInteger(i)  => VFloating(i.toDouble) }
    case (TFloating, TInteger)  => { case VFloating(d) => VInteger(d.toInt) }
    case (TBoolean,  TInteger)  => { case VBoolean(b)  => VInteger(if b then 1 else 0) }
    case (TString,   TInteger)  => { case VString(s)   => 
      s.trim
       .toIntOption
       .map(VInteger.apply)
       .getOrElse(
         content.error(s"Could not cast String '$s' to Integer")
       )}
    case (TString,  TFloating)  => { case VString(s)   => s
      .trim
      .toDoubleOption
      .map(VFloating.apply)
      .getOrElse(
        content.error(s"Could not cast String '$s' to Floating")
      )}
  }

