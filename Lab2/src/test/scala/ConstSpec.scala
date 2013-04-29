import org.scalatest._
import jsy.lab2.ast._
import Lab2._

class ConstSpec extends FlatSpec {

  "ConstDecl" should "extend the environment with the first expression results bound to the identifier, and then evaluate the second expression" in {
    val e1 = N(3)
    val e2 = Binary(Plus, Var("x"), N(1))
    val e3 = evaluate(ConstDecl("x", e1, e2)) 
    assert(e3 === N(4))
  } 
  
}
