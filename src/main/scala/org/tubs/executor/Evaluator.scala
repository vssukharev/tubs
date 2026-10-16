
package org.tubs.executor

import scala.collection.mutable.Map
import scala.util.boundary
import scala.util.boundary.break

import org.tubs.table.*
import org.tubs.view
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

  // ------- Particular evaluation methods --------

  def evalVariable(id: String)(using content: CellContent, context: Table): ExprValue =
    content.getSymbolValue(id) match
      case Some(v) => v
      case None    => content.error(s"Variable $id is not accessible in current scope")

  def evalBinary(op: BinaryOp, nodeA: CellAST, nodeB: CellAST)(using content: CellContent, context: Table): ExprValue =
    import BinaryOp.*
    val a = eval(nodeA)
    val b = eval(nodeB)
    if a == VError || b == VError then VError
    else op match
      // Range
      case Range => evalBinaryStatic[VInteger](a, b)  { (x, y) => VRange(x.v, y.v) }

      // Arithmetic
      case Plus  => evalBinaryStatic[VFloating](a, b) { (x, y) => VFloating(x.v + y.v) }
      case Minus => evalBinaryStatic[VFloating](a, b) { (x, y) => VFloating(x.v - y.v) }
      case Prod  => evalBinaryStatic[VFloating](a, b) { (x, y) => VFloating(x.v * y.v) }
      case Div   => evalBinaryStatic[VFloating](a, b) { (x, y) =>
        if y.v == 0.0 then content.error("Division by zero")
        else VFloating(x.v / y.v)
      }
      case Power => evalBinaryStatic[VFloating](a, b) { (x, y) => VFloating(scala.math.pow(x.v, y.v)) }
      case Mod   => evalBinaryStatic[VInteger](a, b)  { (x, y) => 
        if y.v == 0 then content.error("Modulo by zero") 
        else VInteger(x.v % y.v) 
      }

      // Relational comparisons
      case Lt    => evalBinaryStatic[VFloating](a, b) { (x, y) => VBoolean(x.v < y.v) }
      case Gt    => evalBinaryStatic[VFloating](a, b) { (x, y) => VBoolean(x.v > y.v) }
      case Le    => evalBinaryStatic[VFloating](a, b) { (x, y) => VBoolean(x.v <= y.v) }
      case Ge    => evalBinaryStatic[VFloating](a, b) { (x, y) => VBoolean(x.v >= y.v) }

      // Equality
      case Eq    => evalBinaryEgalitarian(a, b) { (x, y) => VBoolean(x == y) }
      case Neq   => evalBinaryEgalitarian(a, b) { (x, y) => VBoolean(x != y) }

      // Logical
      case And   => evalBinaryStatic[VBoolean](a, b)  { (x, y) => VBoolean(x.v && y.v) }
      case Or    => evalBinaryStatic[VBoolean](a, b)  { (x, y) => VBoolean(x.v || y.v) }

  def evalUnary(op: UnaryOp, node: CellAST)(using CellContent, Table): ExprValue =
    import UnaryOp.*
    val a = eval(node)
    if a == VError then VError
    else op match
      case Plus  => evalUnaryStatic[VFloating](a) { x => VFloating(x.v) }
      case Minus => evalUnaryStatic[VFloating](a) { x => VFloating(-x.v) }
      case Not   => evalUnaryStatic[VBoolean](a)  { x => VBoolean(!x.v) }

  def evalIf(
    cond: CellAST, 
    thenBranch: CellAST, 
    elseBranch: CellAST
  )(
    using CellContent, Table
  ): ExprValue =
    val a = eval(cond)
    if a == VError then 
      VError
    else
      val ap = convertStatic[VBoolean](a)
      if ap.v then eval(thenBranch)
      else        eval(elseBranch)

  def evalFor(
    itName: String, 
    accName: String, 
    initNode: CellAST, 
    rangeNode: CellAST, 
    bodyNode: CellAST
  )(
    using content: CellContent, context: Table
  ): ExprValue =
    boundary:
      val rangeOrig = eval(rangeNode)
      if rangeOrig == VError then break(VError)
      val range = convertStatic[VRange](rangeOrig)

      if range.l > range.r then break(content.error(s"Got from > to when trying to iterate on range ${range.l}..${range.r}"))

      var acc = eval(initNode)
      if acc == VError then break(VError)
      content.pushSymbol(accName, acc)

      var i = range.l
      content.pushSymbol(itName, VInteger(i))

      while i <= range.r do
        acc = eval(bodyNode)
        if acc == VError then break(VError)

        content.changeSymbolValue(accName, acc)
        i += 1
        content.changeSymbolValue(itName, VInteger(i))

      content.popSymbol()
      content.popSymbol() // Return acc in here

  /**
   * Evaluates a reference to a cell. Inner dynamic coordinates are
   * evaluated as integers and are checked on positiveness.
   */
  def evalCellRef(colc: CellCoord, rowc: CellCoord)(using content: CellContent, context: Table): ExprValue = 
    import CellCoord.*

    def coordToIndex(c: CellCoord) = c match
      case Static(i)     => i
      case Dynamic(expr) => convertStatic[VInteger](eval(expr)).v

    val col = coordToIndex(colc)
    val row = coordToIndex(rowc)
    val key = CellKey(col, row)

    if col < 1 then
      content.error(s"Negative column when trying to access cell $key")
    else if row < 1 then
      content.error(s"Negative row when trying to access cell $key")
    else
      val target = context.getCell(key)
      target match
        case Some(target) => 
          target.content.addDependent(content.key)
          target.content.value // Note: here the circular dependencies are handled
        case None => 
          content.error(s"Cell $key is empty and can't be referenced")

  // --------------- Helpers --------------

  /**
   * Evaluates binary expression with resulting type R, in which both
   * operands are egalitarian (equal in the sence of conversion preferences).
   * Resulting type of op is either fixed (when R is one of ExprValue enum values) 
   * or dynamic (when R is ExprValue itself).
   */
  inline def evalBinaryEgalitarian(a: ExprValue, b: ExprValue)[R <: ExprValue](op: (ExprValue, ExprValue) => R)(using content: CellContent, t: Table): ExprValue =
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

  /**
   * Same semantics as for binary but for unary
   */
  inline def evalUnaryStatic[T <: ExprValue](a: ExprValue)[R <: ExprValue](op: T => R)(using CellContent, Table): ExprValue =
    val ap = convertStatic[T](a)
    op(ap)

