package org.tubs.executor

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.tubs.table.CellKey

class CellParserTest extends AnyFunSuite with Matchers {

  val dummyKey = CellKey(1, 1)

  // ==========================================
  // Plain text / Non-formula parsing
  // ==========================================

  test("Strings not starting with '=' are parsed as LitString") {
    CellAST(dummyKey, "hello world") shouldBe CellAST.LitString("hello world")
    CellAST(dummyKey, "12345") shouldBe CellAST.LitString("12345")
    CellAST(dummyKey, "") shouldBe CellAST.LitString("")
    CellAST(dummyKey, "   just text   ") shouldBe CellAST.LitString("   just text   ")
    CellAST(dummyKey, "abc=def") shouldBe CellAST.LitString("abc=def")
  }

  // ==========================================
  // Helper colLettersToIndex tests
  // ==========================================

  test("colLettersToIndex converts column letters to 1-based index") {
    FormulaParser.colLettersToIndex("A") shouldBe 1
    FormulaParser.colLettersToIndex("B") shouldBe 2
    FormulaParser.colLettersToIndex("Z") shouldBe 26
    FormulaParser.colLettersToIndex("AA") shouldBe 27
    FormulaParser.colLettersToIndex("AB") shouldBe 28
    FormulaParser.colLettersToIndex("AZ") shouldBe 52
    FormulaParser.colLettersToIndex("BA") shouldBe 53
    FormulaParser.colLettersToIndex("ZZ") shouldBe 702
    FormulaParser.colLettersToIndex("AAA") shouldBe 703
  }

  // ==========================================
  // Literals & Variables
  // ==========================================

  test("Parse integer literals") {
    CellAST(dummyKey, "=0") shouldBe CellAST.LitInteger(0)
    CellAST(dummyKey, "=42") shouldBe CellAST.LitInteger(42)
    CellAST(dummyKey, "=999999") shouldBe CellAST.LitInteger(999999)
  }

  test("Parse floating literals") {
    CellAST(dummyKey, "=0.0") shouldBe CellAST.LitFloating(0.0)
    CellAST(dummyKey, "=3.14159") shouldBe CellAST.LitFloating(3.14159)
    CellAST(dummyKey, "=123.456") shouldBe CellAST.LitFloating(123.456)
  }

  test("Parse boolean literals") {
    CellAST(dummyKey, "=true") shouldBe CellAST.LitBoolean(true)
    CellAST(dummyKey, "=false") shouldBe CellAST.LitBoolean(false)
  }

  test("Parse variables") {
    CellAST(dummyKey, "=x") shouldBe CellAST.Variable("x")
    CellAST(dummyKey, "=foo") shouldBe CellAST.Variable("foo")
    CellAST(dummyKey, "=var123") shouldBe CellAST.Variable("var123")
    CellAST(dummyKey, "=camelCaseVar") shouldBe CellAST.Variable("camelCaseVar")
  }

  // ==========================================
  // Cell References
  // ==========================================

  test("Parse static cell references") {
    CellAST(dummyKey, "=A1") shouldBe CellAST.CellRef(CellCoord.Static(1), CellCoord.Static(1))
    CellAST(dummyKey, "=B5") shouldBe CellAST.CellRef(CellCoord.Static(2), CellCoord.Static(5))
    CellAST(dummyKey, "=Z26") shouldBe CellAST.CellRef(CellCoord.Static(26), CellCoord.Static(26))
    CellAST(dummyKey, "=AA100") shouldBe CellAST.CellRef(CellCoord.Static(27), CellCoord.Static(100))
  }

  test("Parse static column with dynamic row reference") {
    CellAST(dummyKey, "=A[1]") shouldBe CellAST.CellRef(
      CellCoord.Static(1),
      CellCoord.Dynamic(CellAST.LitInteger(1))
    )
    CellAST(dummyKey, "=B[i + 1]") shouldBe CellAST.CellRef(
      CellCoord.Static(2),
      CellCoord.Dynamic(CellAST.Binary(BinaryOp.Plus, CellAST.Variable("i"), CellAST.LitInteger(1)))
    )
  }

  test("Parse dynamic column with static row reference") {
    CellAST(dummyKey, "=[1]1") shouldBe CellAST.CellRef(
      CellCoord.Dynamic(CellAST.LitInteger(1)),
      CellCoord.Static(1)
    )
    CellAST(dummyKey, "=[col]5") shouldBe CellAST.CellRef(
      CellCoord.Dynamic(CellAST.Variable("col")),
      CellCoord.Static(5)
    )
  }

  test("Parse dynamic column with dynamic row reference") {
    CellAST(dummyKey, "=[1][1]") shouldBe CellAST.CellRef(
      CellCoord.Dynamic(CellAST.LitInteger(1)),
      CellCoord.Dynamic(CellAST.LitInteger(1))
    )
    CellAST(dummyKey, "=[i][j]") shouldBe CellAST.CellRef(
      CellCoord.Dynamic(CellAST.Variable("i")),
      CellCoord.Dynamic(CellAST.Variable("j"))
    )
  }

  // ==========================================
  // Binary Expressions and Operator Precedence
  // ==========================================

  test("Parse arithmetic binary operators") {
    CellAST(dummyKey, "=1 + 2") shouldBe CellAST.Binary(BinaryOp.Plus, CellAST.LitInteger(1), CellAST.LitInteger(2))
    CellAST(dummyKey, "=5 - 3") shouldBe CellAST.Binary(BinaryOp.Minus, CellAST.LitInteger(5), CellAST.LitInteger(3))
    CellAST(dummyKey, "=4 * 2") shouldBe CellAST.Binary(BinaryOp.Prod, CellAST.LitInteger(4), CellAST.LitInteger(2))
    CellAST(dummyKey, "=8 / 4") shouldBe CellAST.Binary(BinaryOp.Div, CellAST.LitInteger(8), CellAST.LitInteger(4))
    CellAST(dummyKey, "=7 mod 3") shouldBe CellAST.Binary(BinaryOp.Mod, CellAST.LitInteger(7), CellAST.LitInteger(3))
  }

  test("Parse power operator with right-associativity") {
    // 2 ^ 3 ^ 2 should parse as 2 ^ (3 ^ 2)
    CellAST(dummyKey, "=2 ^ 3 ^ 2") shouldBe CellAST.Binary(
      BinaryOp.Power,
      CellAST.LitInteger(2),
      CellAST.Binary(BinaryOp.Power, CellAST.LitInteger(3), CellAST.LitInteger(2))
    )
  }

  test("Parse relational comparison operators") {
    CellAST(dummyKey, "=1 < 2") shouldBe CellAST.Binary(BinaryOp.Lt, CellAST.LitInteger(1), CellAST.LitInteger(2))
    CellAST(dummyKey, "=2 > 1") shouldBe CellAST.Binary(BinaryOp.Gt, CellAST.LitInteger(2), CellAST.LitInteger(1))
    CellAST(dummyKey, "=3 <= 3") shouldBe CellAST.Binary(BinaryOp.Le, CellAST.LitInteger(3), CellAST.LitInteger(3))
    CellAST(dummyKey, "=4 >= 4") shouldBe CellAST.Binary(BinaryOp.Ge, CellAST.LitInteger(4), CellAST.LitInteger(4))
    CellAST(dummyKey, "=5 == 5") shouldBe CellAST.Binary(BinaryOp.Eq, CellAST.LitInteger(5), CellAST.LitInteger(5))
    CellAST(dummyKey, "=6 != 7") shouldBe CellAST.Binary(BinaryOp.Neq, CellAST.LitInteger(6), CellAST.LitInteger(7))
  }

  test("Parse logical operators") {
    CellAST(dummyKey, "=true and false") shouldBe CellAST.Binary(BinaryOp.And, CellAST.LitBoolean(true), CellAST.LitBoolean(false))
    CellAST(dummyKey, "=false or true") shouldBe CellAST.Binary(BinaryOp.Or, CellAST.LitBoolean(false), CellAST.LitBoolean(true))
  }

  test("Parse range operator") {
    CellAST(dummyKey, "=1..10") shouldBe CellAST.Binary(BinaryOp.Range, CellAST.LitInteger(1), CellAST.LitInteger(10))
  }

  test("Operator precedence: multiplicative over additive") {
    // 1 + 2 * 3 -> 1 + (2 * 3)
    CellAST(dummyKey, "=1 + 2 * 3") shouldBe CellAST.Binary(
      BinaryOp.Plus,
      CellAST.LitInteger(1),
      CellAST.Binary(BinaryOp.Prod, CellAST.LitInteger(2), CellAST.LitInteger(3))
    )
  }

  test("Operator precedence: power over multiplicative") {
    // 2 * 3 ^ 2 -> 2 * (3 ^ 2)
    CellAST(dummyKey, "=2 * 3 ^ 2") shouldBe CellAST.Binary(
      BinaryOp.Prod,
      CellAST.LitInteger(2),
      CellAST.Binary(BinaryOp.Power, CellAST.LitInteger(3), CellAST.LitInteger(2))
    )
  }

  test("Operator precedence: relational over logical and") {
    // 1 < 2 and 3 > 2 -> (1 < 2) and (3 > 2)
    CellAST(dummyKey, "=1 < 2 and 3 > 2") shouldBe CellAST.Binary(
      BinaryOp.And,
      CellAST.Binary(BinaryOp.Lt, CellAST.LitInteger(1), CellAST.LitInteger(2)),
      CellAST.Binary(BinaryOp.Gt, CellAST.LitInteger(3), CellAST.LitInteger(2))
    )
  }

  test("Parentheses override default precedence") {
    // (1 + 2) * 3
    CellAST(dummyKey, "=(1 + 2) * 3") shouldBe CellAST.Binary(
      BinaryOp.Prod,
      CellAST.Binary(BinaryOp.Plus, CellAST.LitInteger(1), CellAST.LitInteger(2)),
      CellAST.LitInteger(3)
    )
  }

  // ==========================================
  // Unary Expressions
  // ==========================================

  test("Parse unary operators") {
    CellAST(dummyKey, "=+5") shouldBe CellAST.Unary(UnaryOp.Plus, CellAST.LitInteger(5))
    CellAST(dummyKey, "=-5") shouldBe CellAST.Unary(UnaryOp.Minus, CellAST.LitInteger(5))
    CellAST(dummyKey, "=not true") shouldBe CellAST.Unary(UnaryOp.Not, CellAST.LitBoolean(true))
    CellAST(dummyKey, "=not not false") shouldBe CellAST.Unary(
      UnaryOp.Not,
      CellAST.Unary(UnaryOp.Not, CellAST.LitBoolean(false))
    )
  }

  // ==========================================
  // Control Flow: If and For Expressions
  // ==========================================

  test("Parse if expression") {
    CellAST(dummyKey, "=if true then 10 else 20") shouldBe CellAST.IfExpr(
      CellAST.LitBoolean(true),
      CellAST.LitInteger(10),
      CellAST.LitInteger(20)
    )
  }

  test("Parse nested if expression") {
    CellAST(dummyKey, "=if 1 < 2 then if true then 10 else 20 else 30") shouldBe CellAST.IfExpr(
      CellAST.Binary(BinaryOp.Lt, CellAST.LitInteger(1), CellAST.LitInteger(2)),
      CellAST.IfExpr(CellAST.LitBoolean(true), CellAST.LitInteger(10), CellAST.LitInteger(20)),
      CellAST.LitInteger(30)
    )
  }

  test("Parse for expression") {
    CellAST(dummyKey, "=for i, acc=0 in 1..10 do acc + i") shouldBe CellAST.ForExpr(
      "i",
      "acc",
      CellAST.LitInteger(0),
      CellAST.Binary(BinaryOp.Range, CellAST.LitInteger(1), CellAST.LitInteger(10)),
      CellAST.Binary(BinaryOp.Plus, CellAST.Variable("acc"), CellAST.Variable("i"))
    )
  }

  // ==========================================
  // Syntax Errors / Parser Failures
  // ==========================================

  test("Fail on empty formula after '='") {
    CellAST(dummyKey, "=") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=    ") shouldBe CellAST.SyntaxError
  }

  test("Fail on trailing binary operators") {
    CellAST(dummyKey, "=1 +") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=1 * ") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=1 ..") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=true and ") shouldBe CellAST.SyntaxError
  }

  test("Fail on missing left binary operand") {
    CellAST(dummyKey, "=* 5") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=/ 2") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=.. 10") shouldBe CellAST.SyntaxError
  }

  test("Fail on unbalanced parentheses") {
    CellAST(dummyKey, "=(1 + 2") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=1 + 2)") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=((1 + 2)") shouldBe CellAST.SyntaxError
  }

  test("Fail on incomplete if expression") {
    CellAST(dummyKey, "=if true") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=if true then 1") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=if then 1 else 2") shouldBe CellAST.SyntaxError
  }

  test("Fail on incomplete for expression") {
    CellAST(dummyKey, "=for") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=for i in 1..5 do i") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=for i, acc in 1..5 do acc") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=for i, acc=0 in 1..5") shouldBe CellAST.SyntaxError
  }

  test("Fail on reserved keywords used as variable names") {
    CellAST(dummyKey, "=if + 1") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=for = 2") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=mod") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=and") shouldBe CellAST.SyntaxError
  }

  test("Fail on invalid cell reference syntax") {
    CellAST(dummyKey, "=1A") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=A0") shouldBe CellAST.SyntaxError // row index must be >= 1
    CellAST(dummyKey, "=[1]") shouldBe CellAST.SyntaxError
  }

  test("Fail on illegal characters or syntax junk") {
    CellAST(dummyKey, "=@#$") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=1 + ?") shouldBe CellAST.SyntaxError
    CellAST(dummyKey, "=1 + 2 foo") shouldBe CellAST.SyntaxError
  }
}
