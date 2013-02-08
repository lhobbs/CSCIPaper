import org.scalatest._
import jsy.lab2.ast._
import Lab2._

class IfSpec extends FlatSpec {

  "If" should "evaluate the first expression if the conditional is true" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = evaluate(If(B(true), e1, e2)) 
    assert(e3 === N(5))
  } 
  
  "If" should "evaluate the second expression if the conditional is false" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = evaluate(If(B(false), e1, e2)) 
    assert(e3 === N(2))
  } 
  
}
