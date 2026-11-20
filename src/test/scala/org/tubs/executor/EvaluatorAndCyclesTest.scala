package org.tubs.executor

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.tubs.table.{CellKey, Table}

class EvaluatorAndCyclesTest extends AnyFunSuite with Matchers {

  import ExprValue.*

  // ==========================================
  // Basic Evaluation Passes
  // ==========================================

  test("Evaluate literals") {
    val t = Table()
    t.set(CellKey(1, 1), "hello")
    t.getValue(CellKey(1, 1))._1 shouldBe VString("hello")

    t.set(CellKey(1, 2), "=42")
    t.getValue(CellKey(1, 2))._1 shouldBe VInteger(42)

    t.set(CellKey(1, 3), "=3.14")
    t.getValue(CellKey(1, 3))._1 shouldBe VFloating(3.14)

    t.set(CellKey(1, 4), "=true")
    t.getValue(CellKey(1, 4))._1 shouldBe VBoolean(true)

    t.set(CellKey(1, 5), "=false")
    t.getValue(CellKey(1, 5))._1 shouldBe VBoolean(false)
  }

  test("Evaluate arithmetic operations") {
    val t = Table()
    t.set(CellKey(1, 1), "=10 + 20")
    t.getValue(CellKey(1, 1))._1 shouldBe VFloating(30.0)

    t.set(CellKey(1, 2), "=50 - 15")
    t.getValue(CellKey(1, 2))._1 shouldBe VFloating(35.0)

    t.set(CellKey(1, 3), "=6 * 7")
    t.getValue(CellKey(1, 3))._1 shouldBe VFloating(42.0)

    t.set(CellKey(1, 4), "=15 / 3")
    t.getValue(CellKey(1, 4))._1 shouldBe VFloating(5.0)

    t.set(CellKey(1, 5), "=10 mod 3")
    t.getValue(CellKey(1, 5))._1 shouldBe VInteger(1)

    t.set(CellKey(1, 6), "=2 ^ 3")
    t.getValue(CellKey(1, 6))._1 shouldBe VFloating(8.0)

    // Power right-associative: 2 ^ 3 ^ 2 = 2 ^ 9 = 512
    t.set(CellKey(1, 7), "=2 ^ 3 ^ 2")
    t.getValue(CellKey(1, 7))._1 shouldBe VFloating(512.0)
  }

  test("Evaluate unary operations") {
    val t = Table()
    t.set(CellKey(1, 1), "=+42")
    t.getValue(CellKey(1, 1))._1 shouldBe VFloating(42.0)

    t.set(CellKey(1, 2), "=-42")
    t.getValue(CellKey(1, 2))._1 shouldBe VFloating(-42.0)

    t.set(CellKey(1, 3), "=not true")
    t.getValue(CellKey(1, 3))._1 shouldBe VBoolean(false)

    t.set(CellKey(1, 4), "=not false")
    t.getValue(CellKey(1, 4))._1 shouldBe VBoolean(true)
  }

  test("Evaluate relational comparisons") {
    val t = Table()
    t.set(CellKey(1, 1), "=10 < 20")
    t.getValue(CellKey(1, 1))._1 shouldBe VBoolean(true)

    t.set(CellKey(1, 2), "=20 < 10")
    t.getValue(CellKey(1, 2))._1 shouldBe VBoolean(false)

    t.set(CellKey(1, 3), "=15 <= 15")
    t.getValue(CellKey(1, 3))._1 shouldBe VBoolean(true)

    t.set(CellKey(1, 4), "=30 > 25")
    t.getValue(CellKey(1, 4))._1 shouldBe VBoolean(true)

    t.set(CellKey(1, 5), "=30 >= 35")
    t.getValue(CellKey(1, 5))._1 shouldBe VBoolean(false)
  }

  test("Evaluate egalitarian equality and inequality") {
    val t = Table()
    // Same types
    t.set(CellKey(1, 1), "=42 == 42")
    t.getValue(CellKey(1, 1))._1 shouldBe VBoolean(true)

    t.set(CellKey(1, 2), "=42 != 43")
    t.getValue(CellKey(1, 2))._1 shouldBe VBoolean(true)

    t.set(CellKey(1, 3), "=true == true")
    t.getValue(CellKey(1, 3))._1 shouldBe VBoolean(true)

    t.set(CellKey(1, 4), "=true != false")
    t.getValue(CellKey(1, 4))._1 shouldBe VBoolean(true)

    // Subtype promotion: Integer == Floating
    t.set(CellKey(1, 5), "=42 == 42.0")
    t.getValue(CellKey(1, 5))._1 shouldBe VBoolean(true)

    t.set(CellKey(1, 6), "=42 != 42.0")
    t.getValue(CellKey(1, 6))._1 shouldBe VBoolean(false)
  }

  test("Evaluate logical AND and OR") {
    val t = Table()
    t.set(CellKey(1, 1), "=true and true")
    t.getValue(CellKey(1, 1))._1 shouldBe VBoolean(true)

    t.set(CellKey(1, 2), "=true and false")
    t.getValue(CellKey(1, 2))._1 shouldBe VBoolean(false)

    t.set(CellKey(1, 3), "=false or true")
    t.getValue(CellKey(1, 3))._1 shouldBe VBoolean(true)

    t.set(CellKey(1, 4), "=false or false")
    t.getValue(CellKey(1, 4))._1 shouldBe VBoolean(false)
  }

  test("Evaluate range expression") {
    val t = Table()
    t.set(CellKey(1, 1), "=1..10")
    t.getValue(CellKey(1, 1))._1 shouldBe VRange(1, 10)
  }

  test("Evaluate if expressions") {
    val t = Table()
    t.set(CellKey(1, 1), "=if true then 10 else 20")
    t.getValue(CellKey(1, 1))._1 shouldBe VInteger(10)

    t.set(CellKey(1, 2), "=if false then 10 else 20")
    t.getValue(CellKey(1, 2))._1 shouldBe VInteger(20)

    t.set(CellKey(1, 3), "=if 5 > 3 then 100 else 200")
    t.getValue(CellKey(1, 3))._1 shouldBe VInteger(100)

    // Lazy branch evaluation: unselected branch is not evaluated
    t.set(CellKey(1, 4), "=if true then 99 else 1 / 0")
    t.getValue(CellKey(1, 4))._1 shouldBe VInteger(99)

    t.set(CellKey(1, 5), "=if false then 1 / 0 else 88")
    t.getValue(CellKey(1, 5))._1 shouldBe VInteger(88)
  }

  test("Evaluate for expressions") {
    val t = Table()
    // Sum of 1..5 = 15
    t.set(CellKey(1, 1), "=for i, acc=0 in 1..5 do acc + i")
    t.getValue(CellKey(1, 1))._1 shouldBe VFloating(15.0)

    // Factorial of 4 = 24
    t.set(CellKey(1, 2), "=for i, acc=1 in 1..4 do acc * i")
    t.getValue(CellKey(1, 2))._1 shouldBe VFloating(24.0)

    // Single-iteration loop
    t.set(CellKey(1, 3), "=for i, acc=10 in 1..1 do acc + i")
    t.getValue(CellKey(1, 3))._1 shouldBe VFloating(11.0)
  }

  test("Evaluate dynamic cell references") {
    val t = Table()
    t.set(CellKey(1, 1), "=42")
    t.set(CellKey(1, 2), "=100")
    t.set(CellKey(2, 1), "=200")

    // Dynamic row: A[1 + 1] -> A2 (100)
    t.set(CellKey(3, 1), "=A[1 + 1]")
    t.getValue(CellKey(3, 1))._1 shouldBe VInteger(100)

    // Dynamic column: [1 + 1]1 -> B1 (200)
    t.set(CellKey(3, 2), "=[1 + 1]1")
    t.getValue(CellKey(3, 2))._1 shouldBe VInteger(200)

    // Dynamic column and row: [1][1] -> A1 (42)
    t.set(CellKey(3, 3), "=[1][1]")
    t.getValue(CellKey(3, 3))._1 shouldBe VInteger(42)
  }

  // ==========================================
  // Evaluation Fails & Runtime Errors
  // ==========================================

  test("Division by zero returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=10 / 0")
    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("Modulo by zero returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=10 mod 0")
    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("For loop with inverted range (from > to) returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=for i, acc=0 in 5..1 do acc + i")
    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("Accessing variable outside of its scope returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=undefinedVar + 1")
    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("Referencing an empty / non-existent cell returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=B1") // B1 has never been set
    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("Cell coordinates with non-positive index return VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=[0]1")
    t.getValue(CellKey(1, 1))._1 shouldBe VError

    t.set(CellKey(1, 2), "=A[0]")
    t.getValue(CellKey(1, 2))._1 shouldBe VError

    t.set(CellKey(1, 3), "=[-1][-1]")
    t.getValue(CellKey(1, 3))._1 shouldBe VError
  }

  test("Equality between incompatible types returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=\"hello\" == true")
    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("Error propagates through operators") {
    val t = Table()
    t.set(CellKey(1, 1), "=10 / 0") // VError
    t.set(CellKey(1, 2), "=A1 + 5")
    t.getValue(CellKey(1, 2))._1 shouldBe VError
  }

  // ==========================================
  // Cycled Dependencies (Circular Dependency Detection)
  // ==========================================

  test("Direct self-referencing cycle is detected and returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=A1")
    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("Self-reference inside an expression returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=A1 + 10")
    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("Dynamic self-reference returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=[1][1]")
    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("Two-cell mutual dependency cycle returns VError for both cells") {
    val t = Table()
    t.set(CellKey(1, 1), "=B1")
    t.set(CellKey(2, 1), "=A1")

    t.getValue(CellKey(1, 1))._1 shouldBe VError
    t.getValue(CellKey(2, 1))._1 shouldBe VError
  }

  test("Three-cell circular dependency cycle returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=B1")
    t.set(CellKey(2, 1), "=C1")
    t.set(CellKey(3, 1), "=A1")

    t.getValue(CellKey(1, 1))._1 shouldBe VError
    t.getValue(CellKey(2, 1))._1 shouldBe VError
    t.getValue(CellKey(3, 1))._1 shouldBe VError
  }

  test("Circular dependency within complex expressions returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=B1 * 2")
    t.set(CellKey(2, 1), "=C1 + 3")
    t.set(CellKey(3, 1), "=A1 - 1")

    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("Cycle inside for loop body returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=for i, acc=0 in 1..3 do acc + A1")
    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("Cycle inside taken branch of if returns VError") {
    val t = Table()
    t.set(CellKey(1, 1), "=if true then A1 else 0")
    t.getValue(CellKey(1, 1))._1 shouldBe VError
  }

  test("Untaken branch in if does not trigger circular dependency") {
    val t = Table()
    // A1 references A1 in the false branch, but condition is true
    t.set(CellKey(1, 1), "=if true then 42 else A1")
    t.getValue(CellKey(1, 1))._1 shouldBe VInteger(42)
  }

  test("Breaking circular dependency restores correct evaluation") {
    val t = Table()
    t.set(CellKey(1, 1), "=B1")
    t.set(CellKey(2, 1), "=A1")

    // Initially cyclic
    t.getValue(CellKey(1, 1))._1 shouldBe VError

    // Break the cycle by providing a static value for B1
    t.set(CellKey(2, 1), "=100")
    t.getValue(CellKey(2, 1))._1 shouldBe VInteger(100)

    // Now A1 evaluates successfully through B1
    t.getValue(CellKey(1, 1))._1 shouldBe VInteger(100)
  }

  // ==========================================
  // Dependency Tracking & Cascade Invalidation
  // ==========================================

  test("Dependents are tracked when cells are evaluated") {
    val t = Table()
    t.set(CellKey(2, 1), "=50") // B1
    t.set(CellKey(1, 1), "=B1 * 2") // A1

    t.getValue(CellKey(1, 1))._1 shouldBe VFloating(100.0)

    val b1Content = t.getCell(CellKey(2, 1)).get.content
    b1Content.dependents should contain(CellKey(1, 1))
  }

  test("Updating a cell cascades invalidation to all dependent cells") {
    val t = Table()
    t.set(CellKey(3, 1), "=5")       // C1 = 5
    t.set(CellKey(2, 1), "=C1 * 2")   // B1 = 10
    t.set(CellKey(1, 1), "=B1 + 10")  // A1 = 20

    t.getValue(CellKey(1, 1))._1 shouldBe VFloating(20.0)

    // Change C1 to 20
    t.set(CellKey(3, 1), "=20")

    // Now B1 should be 40 and A1 should be 50
    t.getValue(CellKey(2, 1))._1 shouldBe VFloating(40.0)
    t.getValue(CellKey(1, 1))._1 shouldBe VFloating(50.0)
  }

  test("Changing formula cleans up old dependencies") {
    val t = Table()
    t.set(CellKey(2, 1), "=50") // B1
    t.set(CellKey(1, 1), "=B1") // A1

    t.getValue(CellKey(1, 1))._1 shouldBe VInteger(50)
    t.getCell(CellKey(2, 1)).get.content.dependents should contain(CellKey(1, 1))

    // Change A1 to no longer reference B1
    t.set(CellKey(1, 1), "=999")
    t.getValue(CellKey(1, 1))._1 shouldBe VInteger(999)

    // B1's dependents must no longer contain A1
    t.getCell(CellKey(2, 1)).get.content.dependents should not contain(CellKey(1, 1))
  }

  test("Multi-dependency cell updates when either dependency changes") {
    val t = Table()
    t.set(CellKey(1, 2), "=10") // A2
    t.set(CellKey(2, 2), "=20") // B2
    t.set(CellKey(1, 1), "=A2 + B2") // A1

    t.getValue(CellKey(1, 1))._1 shouldBe VFloating(30.0)

    // Update A2
    t.set(CellKey(1, 2), "=30")
    t.getValue(CellKey(1, 1))._1 shouldBe VFloating(50.0)

    // Update B2
    t.set(CellKey(2, 2), "=100")
    t.getValue(CellKey(1, 1))._1 shouldBe VFloating(130.0)
  }
}
