package org.tubs.executor

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import org.tubs.table.{CellKey, Table}

class TypeCheckerTest extends AnyFunSuite with Matchers {

  import ExprType.*
  import ExprValue.*
  import TypeConverter.*
  import TypeConverter.TypeConversion.*

  val dummyKey = CellKey(1, 1)
  given dummyContent: CellContent = CellContent(dummyKey, "")

  // ==========================================
  // ExprType & fromValue tests
  // ==========================================

  test("ExprType.toString should format types appropriately") {
    TBoolean.toString shouldBe "Boolean"
    TInteger.toString shouldBe "Integer"
    TFloating.toString shouldBe "Floating"
    TRange.toString shouldBe "Range"
    TString.toString shouldBe "String"
    TError.toString shouldBe "ErrorType"
  }

  test("ExprType.fromValue should statically map value types to ExprType") {
    ExprType.fromValue[VBoolean] shouldBe TBoolean
    ExprType.fromValue[VInteger] shouldBe TInteger
    ExprType.fromValue[VFloating] shouldBe TFloating
    ExprType.fromValue[VRange] shouldBe TRange
    ExprType.fromValue[VString] shouldBe TString
    ExprType.fromValue[VError.type] shouldBe TError
  }

  test("ExprValue.exprType should return the corresponding ExprType") {
    VBoolean(true).exprType shouldBe TBoolean
    VInteger(42).exprType shouldBe TInteger
    VFloating(3.14).exprType shouldBe TFloating
    VRange(1, 5).exprType shouldBe TRange
    VString("abc").exprType shouldBe TString
    VError.exprType shouldBe TError
  }

  // ==========================================
  // Subtyping relationships
  // ==========================================

  test("Integer is a subtype of Floating, but Floating is not a subtype of Integer") {
    TInteger.isSubtypeOf(TFloating) shouldBe true
    TFloating.isSubtypeOf(TInteger) shouldBe false
  }

  test("Every ExprType is a subtype of itself") {
    TBoolean.isSubtypeOf(TBoolean) shouldBe true
    TInteger.isSubtypeOf(TInteger) shouldBe true
    TFloating.isSubtypeOf(TFloating) shouldBe true
    TRange.isSubtypeOf(TRange) shouldBe true
    TString.isSubtypeOf(TString) shouldBe true
    TError.isSubtypeOf(TError) shouldBe true
  }

  test("Subtype comparison returns correct ordering") {
    // -1 means at is subtype of bt
    subtypesCompare(TInteger, TFloating) shouldBe -1
    // 1 means bt is subtype of at
    subtypesCompare(TFloating, TInteger) shouldBe 1
    // 0 means no subtype relationship
    subtypesCompare(TBoolean, TInteger) shouldBe 0
    subtypesCompare(TString, TInteger) shouldBe 0
    subtypesCompare(TRange, TFloating) shouldBe 0
  }

  // ==========================================
  // TypeConverter passes (Available)
  // ==========================================

  test("Convert to same type is always Available") {
    val intConv = convert(VInteger(42), TInteger)
    intConv shouldBe a[Available]
    intConv.value shouldBe VInteger(42)

    val floatConv = convert(VFloating(3.14), TFloating)
    floatConv shouldBe a[Available]
    floatConv.value shouldBe VFloating(3.14)

    val boolConv = convert(VBoolean(true), TBoolean)
    boolConv shouldBe a[Available]
    boolConv.value shouldBe VBoolean(true)

    val strConv = convert(VString("hello"), TString)
    strConv shouldBe a[Available]
    strConv.value shouldBe VString("hello")

    val rangeConv = convert(VRange(1, 10), TRange)
    rangeConv shouldBe a[Available]
    rangeConv.value shouldBe VRange(1, 10)
  }

  test("Convert Integer to Floating succeeds") {
    val conv1 = convert(VInteger(42), TFloating)
    conv1 shouldBe a[Available]
    conv1.value shouldBe VFloating(42.0)

    val conv2 = convert(VInteger(-15), TFloating)
    conv2.value shouldBe VFloating(-15.0)
  }

  test("Convert Floating to Integer succeeds with truncation") {
    val conv1 = convert(VFloating(42.8), TInteger)
    conv1 shouldBe a[Available]
    conv1.value shouldBe VInteger(42)

    val conv2 = convert(VFloating(-3.99), TInteger)
    conv2.value shouldBe VInteger(-3)
  }

  test("Convert Boolean to Integer succeeds (true -> 1, false -> 0)") {
    val convTrue = convert(VBoolean(true), TInteger)
    convTrue.value shouldBe VInteger(1)

    val convFalse = convert(VBoolean(false), TInteger)
    convFalse.value shouldBe VInteger(0)
  }

  test("Convert valid String to Integer succeeds with trimming") {
    val conv1 = convert(VString("123"), TInteger)
    conv1.value shouldBe VInteger(123)

    val conv2 = convert(VString("  -99  "), TInteger)
    conv2.value shouldBe VInteger(-99)
  }

  test("Convert valid String to Floating succeeds with trimming") {
    val conv1 = convert(VString("3.14159"), TFloating)
    conv1.value shouldBe VFloating(3.14159)

    val conv2 = convert(VString("  -0.5  "), TFloating)
    conv2.value shouldBe VFloating(-0.5)
  }

  test("convertUnwrap and convertStatic return evaluated ExprValue") {
    convertUnwrap(VInteger(10), TFloating) shouldBe VFloating(10.0)
    convertStatic[VFloating](VInteger(25)) shouldBe VFloating(25.0)
    convertStatic[VInteger](VFloating(9.99)) shouldBe VInteger(9)
  }

  // ==========================================
  // TypeConverter fails
  // ==========================================

  test("Incompatible type conversions return Fail") {
    convert(VInteger(1), TBoolean) shouldBe Fail
    convert(VFloating(1.0), TBoolean) shouldBe Fail
    convert(VBoolean(true), TFloating) shouldBe Fail
    convert(VBoolean(true), TRange) shouldBe Fail
    convert(VRange(1, 5), TInteger) shouldBe Fail
    convert(VRange(1, 5), TFloating) shouldBe Fail
    convert(VRange(1, 5), TBoolean) shouldBe Fail
    convert(VString("true"), TBoolean) shouldBe Fail
    convert(VString("1..5"), TRange) shouldBe Fail
    convert(VInteger(1), TRange) shouldBe Fail
  }

  test("Fail.value throws NoSuchElementException") {
    an[NoSuchElementException] should be thrownBy {
      Fail.value
    }
  }

  test("Invalid String content fails during conversion to Integer or Floating") {
    val intConv = convert(VString("notAnInt"), TInteger)
    intConv shouldBe a[Available]
    intConv.value shouldBe VError

    val floatConv = convert(VString("abc"), TFloating)
    floatConv shouldBe a[Available]
    floatConv.value shouldBe VError
  }

  test("convertUnwrap on Fail returns VError") {
    convertUnwrap(VBoolean(true), TRange) shouldBe VError
    convertUnwrap(VInteger(100), TBoolean) shouldBe VError
  }

  // ==========================================
  // ExprValue monadic and utility methods
  // ==========================================

  test("ExprValue.map transforms valid values and propagates VError") {
    VInteger(5).map(v => VInteger(v.unbox[VInteger].v * 2)) shouldBe VInteger(10)
    VError.map(v => VInteger(99)) shouldBe VError
  }

  test("ExprValue.flatMap transforms valid values and propagates VError") {
    VInteger(5).flatMap(v => VInteger(v.unbox[VInteger].v + 1)) shouldBe VInteger(6)
    VError.flatMap(v => VInteger(99)) shouldBe VError
  }

  test("ExprValue.withFilter filters values as VError if predicate fails") {
    VInteger(5).withFilter(_.unbox[VInteger].v > 0) shouldBe VInteger(5)
    VInteger(-5).withFilter(_.unbox[VInteger].v > 0) shouldBe VError
    VError.withFilter(_ => true) shouldBe VError
  }

  test("ExprValue.unbox casts safely to target subtype") {
    val v: ExprValue = VInteger(123)
    val unboxed = v.unbox[VInteger]
    unboxed.v shouldBe 123
  }

  // ==========================================
  // ExprValueOpt tests
  // ==========================================

  test("ExprValueOpt toOption and apply constructors") {
    ExprValueOpt.Empty.toOption shouldBe None
    ExprValueOpt.Pending.toOption shouldBe None
    ExprValueOpt.Result(VInteger(42)).toOption shouldBe Some(VInteger(42))

    ExprValueOpt(None) shouldBe ExprValueOpt.Empty
    ExprValueOpt(Some(VString("res"))) shouldBe ExprValueOpt.Result(VString("res"))
  }
}
