import org.scalatest._
import jsy.lab4.ast._
import Lab4._

class ConstSpec extends FlatSpec {


  "ConstDecl" should "perform a substitution using small-step semantics" in {
    val e1 = N(3)
    val e2 = Binary(Plus, Var("x"), N(1))
    val e3 = iterateStep(ConstDecl("x", e1, e2)) 
    assert(e3 === N(4))
  } 

  
}
