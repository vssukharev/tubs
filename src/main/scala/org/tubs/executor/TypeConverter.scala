
package org.tubs.executor

import scala.compiletime.erasedValue

object TypeConverter:
  import ExprValue.*
  import ExprType.*
  import TypeConversion.*

  /**
   * Converts source value sourceV to target type targetT.
   * If conversion is successfull, lazily computes it's result.
   * Same types are also accepted and signalized with special
   * value Same.
   */
  def convert(sourceV: ExprValue, targetT: ExprType): TypeConversion =
    val sourceT = sourceV.exprType
    if (sourceT == targetT) then 
      Available(() => sourceV)
    else
      conversionTable.get((sourceT, targetT)) match
        case Some(f) => Available(() => f(sourceV))
        case None    => Fail

  /**
   * Converts and automatically unwraps expression values. Also can be called
   * with already wrapped value.
   */
  def convertUnwrap(sourceV: ExprValue, targetT: ExprType)(using content: CellContent): ExprValue =
    convert(sourceV, targetT) match 
      case Available(t) => t()
      case Fail         => content.error(s"Unable to convert from type ${sourceV.exprType} to ${targetT} during formula calculations.")

  /**
   * Statically convert sourceV to type T and do the unwrapping checks.
   */
  inline def convertStatic[T <: ExprValue](sourceV: ExprValue)(using content: CellContent): T =
    convertUnwrap(sourceV, ExprType.fromValue[T]).asInstanceOf[T]

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

  type Converter = PartialFunction[ExprValue, ExprValue]

  val conversionTable: Map[(ExprType, ExprType), Converter] = Map(
    (TInteger,  TFloating) -> { case VInteger(i)  => VFloating(i.toDouble) },
    (TFloating, TInteger)  -> { case VFloating(d) => VInteger(d.toInt) },
    (TInteger,  TBoolean)  -> { case VInteger(i)  => VBoolean(i != 0) },
    (TBoolean,  TInteger)  -> { case VBoolean(b)  => VInteger(if b then 1 else 0) },
    (TString,   TInteger)  -> { case VString(s)   => 
      s.trim.toIntOption.map(VInteger.apply).getOrElse(VError)
    },
    (TString,   TFloating) -> { case VString(s)   => 
      s.trim.toDoubleOption.map(VFloating.apply).getOrElse(VError)
    }
  )


