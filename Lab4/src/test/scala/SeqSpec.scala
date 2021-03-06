import org.scalatest._
import jsy.lab4.ast._
import Lab4._

class SeqSpec extends FlatSpec {

  "Seq" should "execute both expressions using small-step semantics" in {
    val e1 = Binary(Plus, N(3), N(2))
    val e2 = Binary(Plus, N(1), N(1))
    val e3 = iterateStep(Binary(Seq, e1, e2)) 
    assert(e3 === N(2))
  } 
  
}
