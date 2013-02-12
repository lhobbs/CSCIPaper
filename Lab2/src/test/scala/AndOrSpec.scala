import org.scalatest._
import jsy.lab2.ast._
import Lab2._

class AndOrSpec extends FlatSpec {

  "And" should "return true only if both expressions are true" in {
    val t = B(true)
    val f = B(false)
    assert(evaluate(Binary(And,t,t)) === t)
    assert(evaluate(Binary(And,t,f)) === f)
    assert(evaluate(Binary(And,f,t)) === f)
    assert(evaluate(Binary(And,f,f)) === f)
  } 
 
  "And" should "return non-intuitive results from differing types" in {
    val e1 = N(0)
    val e2 = B(true)
    val e3 = evaluate(Binary(And, e1, e2))
    assert(e3 === N(0))
  }
 
  "Or" should "return true if either or both expressions are true" in {
    val t = B(true)
    val f = B(false)
    assert(evaluate(Binary(Or,t,t)) === t)
    assert(evaluate(Binary(Or,f,t)) === t)
    assert(evaluate(Binary(Or,t,f)) === t)
    assert(evaluate(Binary(Or,f,f)) === f)
  }

  "Or" should "return non-intuitive results from differing types" in {
    val e1 = N(5)
    val e2 = B(false)
    val e3 = evaluate(Binary(Or, e1, e2))
    assert(e3 === N(5))
  }
  
}
