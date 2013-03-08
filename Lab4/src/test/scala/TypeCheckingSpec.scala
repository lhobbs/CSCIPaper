import org.scalatest._
import jsy.lab4.ast._
import Lab4._

class TypeCheckingSpec extends FlatSpec {

  "Type Checking" should "determine the type of Numbers as TNumber" in 
  {
    assert(inferType(N(5)) == TNumber)
  } 

  "Type Checking" should "determine the type of Strings as TString" in 
  {
    assert(inferType(S("Hello World")) == TString)
  }

  "Type Checking" should "determine the type of Booleans as TBool" in 
  {
    assert(inferType(B(true)) == TBool)
  }

  "Type Checking" should "determine the type of Undefined as TUndefined" in
  {
    assert(inferType(Undefined) == TUndefined)
  }

  "Type Checking" should "determine the type of a negative number as TNumber" in
  {
    assert(inferType(Unary(Neg, N(5))) == TNumber)
  }    

  "Type Checking" should "determine the type of arithmetic operations as TNumber" in
  { 
    assert(inferType(Binary(Plus, N(1), N(1))) == TNumber)
    assert(inferType(Binary(Minus, N(1), N(1))) == TNumber)
    assert(inferType(Binary(Times, N(1), N(1))) == TNumber)
    assert(inferType(Binary(Div, N(1), N(1))) == TNumber)
  }

  "Type Checking" should "not allow arithmetic operations on non-numbers" in
  {
    intercept[StaticTypeError] {
      inferType(Unary(Neg, B(true)))
    }
    intercept[StaticTypeError] {
      inferType(Binary(Plus, N(1), S("a")))
    }
    intercept[StaticTypeError] {
      inferType(Binary(Minus, N(1), B(true)))
    }
    intercept[StaticTypeError] {
      inferType(Binary(Times, N(1), Undefined))
    }
    intercept[StaticTypeError] {
      inferType(Binary(Div, N(1), Function(None, List(), None, N(1))))
    }

  }

  "Type Checking" should "allow string concatenation" in
  {
    assert(inferType(Binary(Plus, S("a"), S("b"))) == TString)
  }

  "Type Checking" should "determine the type of conditional expressions as TBool" in
  {
    assert(inferType(Binary(Eq, N(1), N(2))) == TBool)
    assert(inferType(Binary(Ne, S("a"), S("b"))) == TBool)
    assert(inferType(Binary(Gt, N(1), N(2))) == TBool)
    assert(inferType(Binary(Ge, S("a"), S("b"))) == TBool)
    assert(inferType(Binary(Lt, N(1), N(2))) == TBool)
    assert(inferType(Binary(Le, N(2), N(2))) == TBool)
  }

  "Type Checking" should "determine the type of And and Or expressions as TBool" in
  {
    assert(inferType(Binary(And, B(true), B(false))) == TBool)
    assert(inferType(Binary(Or, B(true), B(true))) == TBool)
  }

  "Type Checking" should "not allow type inference" in
  {
    intercept[StaticTypeError] {
      inferType(Binary(Plus, N(5), S("a")))
    }
    intercept[StaticTypeError] {
      inferType(Binary(Plus, B(true), N(1)))
    }
    intercept[StaticTypeError] {
      inferType(Binary(Plus, Undefined, N(1)))
    }
    intercept[StaticTypeError] {
      inferType(Binary(Plus, S("a"), Function(None, List(("x", TBool)), None, Undefined)))
    } 
  }

  "Type Checking" should "ensure that sequence check both expressions for a valid type" in
  {
    assert(inferType(Binary(Seq, N(1), N(2))) == TNumber)
    intercept[StaticTypeError] {
      inferType(Binary(Seq, Binary(Plus, N(1), Undefined), N(2)))
    }
  }

  "Type Checking" should "ensure then and else cases have the same type" in
  {
    assert(inferType(If(B(true), N(1), N(2))) == TNumber)
    assert(inferType(If(B(false), N(1), N(2))) == TNumber)
    intercept[StaticTypeError] {
      inferType(If(Undefined, N(1), N(2)))
    }
    intercept[StaticTypeError] {
      inferType(If(B(true), N(1), S("a")))
    }
    intercept[StaticTypeError] {
      inferType(If(B(false), N(1), S("a")))
    }
  }

  "Type Checking" should "make sure that call matches function parameters and arguements" in
  {
    assert(inferType(Call(Function(None, List(("x", TNumber), ("y", TBool)), None, Undefined), List(N(1), B(true)))) == TUndefined)
    intercept[StaticTypeError] {
      inferType(Call(Function(None, List(("x", TNumber), ("y", TBool)), None, Undefined), List(N(1), N(2))))
    }
  }

  "Type checking" should "ensure that callis provide the same number of arguments as parameters in the function" in
  {
    intercept[StaticTypeError] {
      inferType(Call(Function(None, List(("x", TBool), ("y", TBool)), None, Undefined), List(B(true), B(false), B(true))))
    }
    intercept[StaticTypeError] {
      inferType(Call(Function(None, List(("x", TBool), ("y", TBool)), None, Undefined), List(B(true))))
    }
  }

  "Type Checking" should "ensure that annotated functions return the correct type" in
  {
    assert(inferType(Function(None, List(), Some(TNumber), N(1))) == TFunction(List(), TNumber))
    intercept[StaticTypeError] { 
      inferType(Function(None, List(), Some(TBool), N(1)))
    }
  }

  "Type Checking" should "check types of the fields of objects" in
  {
    val obj = Obj(Map("a" -> N(5.0), "b" -> N(2.0)))

    assert(inferType(obj) == TObj(Map("a" -> TNumber, "b" -> TNumber)))
    assert(inferType(GetField(obj, "a")) == TNumber)
    intercept[StaticTypeError] {
      inferType(GetField(obj, "c"))
    }
  }
}
