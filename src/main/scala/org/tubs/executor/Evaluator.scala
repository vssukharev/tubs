
package org.tubs.executor

import org.tubs.table.*
import org.tubs.view
import scala.collection.mutable.Map
import org.tubs.executor.TypeConverter.*

/**
 * Organizes the table evaluation process, performing dynamic
 * type check via type checker and then calculating the result.
 * Also handles circular dependencies on cells.
 */
object Evaluator:
  import ExprValue.*
  import ExprType.*
  import CellAST.*
  import TypeConverter.TypeConversion.*

  /**
   * Extracts built AST, evaluates all given expressions 
   * and follows the referenced cells, recursively
   * calling itself. Used to evaluate top-level expressions
   * (for instance, when the formula is changed).
   *
   * In case of error, returns ExprValue.VError, which leads
   * all dependent cells to also have ExprValue.VError.
   */
  def eval(content: CellContent)(using Table): ExprValue =
    eval(content.ast)(using content)

  /**
   * Practical eval method overloading for internal use
   */
  def eval(ast: CellAST)(using CellContent, Table): ExprValue =
    ast match
      case LitString(str)               => VString(str)
      case LitInteger(v)                => VInteger(v)
      case LitFloating(v)               => VFloating(v)
      case LitBoolean(v)                => VBoolean(v)
      case Variable(id)                 => evalVariable(id)
      case Binary(op, a, b)             => evalBinary(op, a, b)
      case Unary(op, a)                 => evalUnary(op, a)
      case CellRef(col, row)            => evalCellRef(col, row)
      case IfExpr(cond, t, e)           => evalIf(cond, t, e)
      case ForExpr(it, acc, init, r, b) => evalFor(it, acc, init, r, b)
      case SyntaxError                  => VError

  def evalVariable(id: String)(using content: CellContent, context: Table): ExprValue =
    content.getSymbolValue(id) match
      case Some(v) => v
      case None    => content.error(s"Variable $id is not accessible in current scope")

  def evalBinary(op: BinaryOp, nodeA: CellAST, nodeB: CellAST)(using CellContent, Table): ExprValue =
    import BinaryOp.*
    val a = eval(nodeA)
    val b = eval(nodeB)
    if a == VError || b == VError then VError
    else op match
      case BinaryOp.Range => evalBinaryStatic[VInteger](a, b)  { (x, y) => VRange(x.v, y.v)     }
      case Plus           => evalBinaryStatic[VFloating](a, b) { (x, y) => VFloating(x.v + y.v) }

  /**
   * Evaluates binary expression with resulting type R, in which both
   * operands are egalitarian (equal in the sence of conversion preferences).
   * Resulting type of op is either fixed (when R is one of ExprValue enum values) 
   * or dynamic (when R is ExprValue itself).
   */
  inline def evalBinaryEgalitarian[R <: ExprValue](a: ExprValue, b: ExprValue, op: (ExprValue, ExprValue) => R)(using content: CellContent, t: Table): ExprValue =
    val at = a.exprType
    val bt = b.exprType
    if at == bt then
      op(a, b)
    else
      val ap = convert(a, bt)
      val bp = convert(b, at)
      (ap, bp) match
        case (Available(_), Available(_)) => content.error(s"Ambiguous conversion between types $at and $bt in binary operation.")
        case (Available(t), Fail)         => op(t(), b)
        case (Fail, Available(t))         => op(a, t())
        case (Fail, Fail)                 => content.error(s"Incompatible types $at and $bt in binary operation.")

  /**
   * Evaluates binary expression, whose operand types are fixed (static) and should
   * be known for any input.
   */
  inline def evalBinaryStatic[T <: ExprValue](a: ExprValue, b: ExprValue)[R <: ExprValue](op: (T, T) => R)(using CellContent, Table): ExprValue =
    val ap = convertStatic[T](a)
    val bp = convertStatic[T](b)
    op(ap, bp)

  def evalUnary(op: UnaryOp, node: CellAST)(using CellContent, Table): ExprValue = VError
  def evalIf(cond: CellAST, thenBranch: CellAST, elseBranch: CellAST): ExprValue  = VError
  def evalFor(it: String, acc: String, init: CellAST, range: CellAST, body: CellAST): ExprValue = VError

  def evalCellRef(colc: CellCoord, rowc: CellCoord)(using content: CellContent, context: Table): ExprValue = 
    // def coordToIndex(c: CellCoord) = c match
    //   case Static(i)     => i
    //   case Dynamic(expr) => eval(expr)

    VError
    // val col = coordToIndex(colc)
    // val row = coordToIndex(rowc)
    // val key = CellKey(col, row)
    // val target = context.get(key)
    // target match
    //   case Some(target) => 
    //     target.content.addDependent(content.key)
    //     target.content.value // Note: here the circular dependencies are handled
    //   case None => content.error(s"Cell $key is empty and can't be referenced")


