import org.scalatest._
import jsy.lab4.ast._
import Lab4._

class ObjectSpec extends FlatSpec {

  "Objects" should "map fields to values" in {
    val obj = Obj(Map("a" -> N(1.0)))
    assert(iterateStep(obj) == Obj(Map("a" -> N(1.0))))
  }

  "Objects" should "perform step on fields" in {
    assert(iterateStep(Obj(Map("a" -> Unary(Neg, N(1.0))))) == Obj(Map("a" -> N(-1.0))))
  }

  "GetField" should "fetch a field if it's in the object" in {
    val obj = Obj(Map("a" -> N(1.0), "b" -> N(2.0)))
    assert(iterateStep(GetField(obj, "a")) == N(1.0))
  }

  "GetField" should "get stuck if trying to get a non-existant field" in {
    val obj = Obj(Map("a" -> N(1.0), "b" -> N(2.0)))
    intercept [StuckError] {
      iterateStep(GetField(obj, "c"))
    }
  }
}
