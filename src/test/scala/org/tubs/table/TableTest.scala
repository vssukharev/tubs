package org.tubs.table

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import upickle.default.*
import scalafx.scene.paint.Color

import org.tubs.executor.{CellContent, ExprValue}

class TableTest extends AnyFunSuite with Matchers {

  // ==========================================
  // CellKey tests
  // ==========================================

  test("CellKey.toString should format coordinates in standard spreadsheet notation") {
    CellKey(1, 1).toString shouldBe "A1"
    CellKey(2, 5).toString shouldBe "B5"
    CellKey(26, 10).toString shouldBe "Z10"
    CellKey(27, 1).toString shouldBe "AA1"
    CellKey(28, 20).toString shouldBe "AB20"
    CellKey(52, 99).toString shouldBe "AZ99"
    CellKey(53, 100).toString shouldBe "BA100"
    CellKey(702, 1).toString shouldBe "ZZ1"
    CellKey(703, 1).toString shouldBe "AAA1"
  }

  test("CellKey ordering should compare column first, then row") {
    val ord = summon[Ordering[CellKey]]
    ord.compare(CellKey(1, 1), CellKey(1, 2)) should be < 0
    ord.compare(CellKey(1, 2), CellKey(1, 1)) should be > 0
    ord.compare(CellKey(1, 1), CellKey(1, 1)) shouldBe 0

    ord.compare(CellKey(1, 10), CellKey(2, 1)) should be < 0
    ord.compare(CellKey(2, 1), CellKey(1, 10)) should be > 0

    val unsorted = List(CellKey(2, 2), CellKey(1, 5), CellKey(2, 1), CellKey(1, 1))
    unsorted.sorted shouldBe List(CellKey(1, 1), CellKey(1, 5), CellKey(2, 1), CellKey(2, 2))
  }

  test("CellKey implicit conversion from (Int, Int) tuple") {
    import scala.language.implicitConversions
    import CellKey.given
    val key: CellKey = (3, 7)
    key shouldBe CellKey(3, 7)
    key.colId shouldBe 3
    key.rowId shouldBe 7
  }

  test("CellKey serialization round-trip with uPickle") {
    val key = CellKey(5, 42)
    val json = write(key)
    val deserialized = read[CellKey](json)
    deserialized shouldBe key
  }

  // ==========================================
  // Table public methods tests
  // ==========================================

  test("Table starts empty with no cells, rows, or cols") {
    val t = Table()
    t.cells shouldBe empty
    t.rows shouldBe empty
    t.cols shouldBe empty
    t.getCell(CellKey(1, 1)) shouldBe None
    t.getFormula(CellKey(1, 1)) shouldBe ""
    t.getValue(CellKey(1, 1)) shouldBe ((ExprValue.VString(""), Set.empty))
  }

  test("Table.set and getCell should store and retrieve cells") {
    val t = Table()
    val cell = t.set(CellKey(1, 1), "hello")

    cell.content.key shouldBe CellKey(1, 1)
    cell.content.formula shouldBe "hello"

    t.getCell(CellKey(1, 1)) shouldBe Some(cell)
    t.getFormula(CellKey(1, 1)) shouldBe "hello"
  }

  test("Table.set with explicit ExprValue") {
    val t = Table()
    val cell = t.set(CellKey(1, 1), "=10", ExprValue.VInteger(10))

    cell.content.formula shouldBe "=10"
    t.getValue(CellKey(1, 1))._1 shouldBe ExprValue.VInteger(10)
  }

  test("Table.getFormula returns empty string for non-existent cell") {
    val t = Table()
    t.getFormula(CellKey(99, 99)) shouldBe ""
  }

  test("Table.getAppearance returns cell appearance if present or default if non-existent") {
    val t = Table()
    val defaultApp = t.getAppearance(CellKey(5, 5))
    defaultApp shouldBe CellAppearance()

    val cell = t.set(CellKey(1, 1), "text")
    cell.appearance = CellAppearance(Color.Red, Color.Blue)

    t.getAppearance(CellKey(1, 1)) shouldBe CellAppearance(Color.Red, Color.Blue)
  }

  test("Table.getValue returns evaluated value and dependent cells") {
    val t = Table()
    t.set(CellKey(1, 1), "=10 + 20")
    val (value, deps) = t.getValue(CellKey(1, 1))
    value shouldBe ExprValue.VFloating(30.0)
    deps shouldBe empty

    // Non-existent cell returns empty VString and empty dependents
    val (emptyVal, emptyDeps) = t.getValue(CellKey(10, 10))
    emptyVal shouldBe ExprValue.VString("")
    emptyDeps shouldBe empty
  }

  test("Table.set overwriting an existing cell refreshes its formula and value") {
    val t = Table()
    t.set(CellKey(1, 1), "=10 + 20")
    t.getValue(CellKey(1, 1))._1 shouldBe ExprValue.VFloating(30.0)

    t.set(CellKey(1, 1), "=5 * 5")
    t.getFormula(CellKey(1, 1)) shouldBe "=5 * 5"
    t.getValue(CellKey(1, 1))._1 shouldBe ExprValue.VFloating(25.0)
  }

  test("Table JSON serialization and deserialization preserves all cells and structures") {
    val t = Table()
    t.rows += Row(100.0)
    t.cols += Column(80.0)
    t.set(CellKey(1, 1), "text cell")
    t.set(CellKey(1, 2), "=42", ExprValue.VInteger(42))
    t.getCell(CellKey(1, 1)).get.appearance = CellAppearance(Color.Green, Color.Yellow)

    val json = write(t, 2)
    val restored = read[Table](json)

    restored.rows.length shouldBe 1
    restored.rows.head.width shouldBe 100.0

    restored.cols.length shouldBe 1
    restored.cols.head.width shouldBe 80.0

    restored.getFormula(CellKey(1, 1)) shouldBe "text cell"
    restored.getAppearance(CellKey(1, 1)) shouldBe CellAppearance(Color.Green, Color.Yellow)

    restored.getFormula(CellKey(1, 2)) shouldBe "=42"
    restored.getValue(CellKey(1, 2))._1 shouldBe ExprValue.VInteger(42)
  }

  test("Table.toString returns non-empty JSON string") {
    val t = Table()
    t.set(CellKey(1, 1), "sample")
    t.toString should include("cells")
    t.toString should include("sample")
  }
}
