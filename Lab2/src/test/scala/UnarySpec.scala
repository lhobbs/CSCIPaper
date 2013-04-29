import org.scalatest._
import jsy.lab2.ast._
import Lab2._

class UnarySpec extends FlatSpec {

  "Neg" should "return the negation of a number value" in {
    val e1 = N(5)
    val e2 = evaluate(Unary(Neg, e1))
    assert(e2 === N(-5))
  } 
  
  "Not" should "return the compliment of a boolean value" in {
    val e1 = B(true)
    val e2 = B(false)
    val e3 = evaluate(Unary(Not, e1))
    val e4 = evaluate(Unary(Not, e2))
    assert(e3 === B(false))
    assert(e4 === B(true))
  }
}
