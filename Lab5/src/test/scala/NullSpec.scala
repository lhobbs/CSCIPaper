import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class NullSpec extends FlatSpec {

  "Null" should "have type TNull " in {
    assert(typeInfer(Map.empty, Null) == TNull)    
  }

  "Null" should "be cast when assigned to an object type " in {
    val e1 = Call(Function(None, List(), None, Unary(Cast(TObj(Map("x" -> TNumber))), Null)), List())
    assert(iterateStep(Map.empty, e1) == (Map.empty, Null))
  }

  "Null" should "throw an error if dereferenced " in {
    val e1 = GetField(Unary(Cast(TObj(Map("x" -> TNumber))), Null), "x")
    intercept[NullDereferenceError] {
      iterateStep(Map.empty, e1)
    }
  }
}
