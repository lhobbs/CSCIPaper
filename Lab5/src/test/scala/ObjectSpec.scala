import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class ObjectSpec extends FlatSpec {

  "Objects" should "map fields to values" in {
    A.reset()
    val obj = Obj(Map("a" -> N(1.0)))
    assert(iterateStep(Map.empty, obj) == (Map(A(1) -> obj), A(1)))
  }

  "GetField" should "fetch a field if it's in the object" in {
    A.reset()
    val obj = Obj(Map("a" -> N(1.0), "b" -> N(2.0)))
    val result = iterateStep(Map.empty, GetField(obj, "a"))
    assert(result == (Map(A(1) -> obj), N(1.0)))
   }

  "Objects" should "perform step on fields" in {
    A.reset()
    val obj = Obj(Map("a" -> Unary(Neg, N(1.0))))
    val obj1 = Obj(Map("a" -> N(-1.0)))
    assert(iterateStep(Map.empty, obj) == (Map(A(1)->obj1),A(1)))
  }

  "GetField" should "get stuck if trying to get a non-existant field" in {
    A.reset()
    val obj = Obj(Map("a" -> N(1.0), "b" -> N(2.0)))
    intercept [StuckError] {
      iterateStep(Map.empty, GetField(obj, "c"))
    }
  }
}
