import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class UnarySpec extends FlatSpec {

  "Neg" should "return the negation of a number value using small-step semantics" in {
    val e1 = N(5)
    val e2 = iterateStep(Map.empty, Unary(Neg, e1))
    assert(e2 === (Map.empty, N(-5)))
  } 
  
  "Not" should "return the compliment of a boolean value using small-step semantics" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = iterateStep(Map.empty, Unary(Not, e1))
    val e4 = iterateStep(Map.empty, Unary(Not, e2))
    assert(e3 === (Map.empty, B(false)))
    assert(e4 === (Map.empty, B(true)))
  }
}
