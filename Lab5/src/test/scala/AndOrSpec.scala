import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class AndOrSpec extends FlatSpec {


  "And" should "return true only if both expressions are true using small-step semantics" in {
    val t = B(true)
    val f =  B(false)
    assert(iterateStep(Map.empty, Binary(And,t,t)) === (Map.empty, t))
    assert(iterateStep(Map.empty, Binary(And,t,f)) === (Map.empty, f))
    assert(iterateStep(Map.empty, Binary(And,f,t)) === (Map.empty, f))
    assert(iterateStep(Map.empty, Binary(And,f,f)) === (Map.empty, f))
  }

  "Or" should "return true if either or both expressions are true using small-step semantics" in {
    val t = B(true)
    val f = B(false)
    assert(iterateStep(Map.empty, Binary(Or,t,t)) === (Map.empty, t))
    assert(iterateStep(Map.empty, Binary(Or,f,t)) === (Map.empty, t))
    assert(iterateStep(Map.empty, Binary(Or,t,f)) === (Map.empty, t))
    assert(iterateStep(Map.empty, Binary(Or,f,f)) === (Map.empty, f))
  }
}
