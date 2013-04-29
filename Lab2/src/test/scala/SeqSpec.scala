import org.scalatest._
import jsy.lab2.ast._
import Lab2._

class SeqSpec extends FlatSpec {

  "Seq" should "execute the first expression, followed by the second, and should evaluate to the second expression" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = evaluate(Binary(Seq, e1, e2)) 
    assert(e3 === N(2))
  } 
  
}
