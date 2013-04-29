import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class DeclSpec extends FlatSpec {


  "Decl(Const, x, e1, e2)" should "replace ConstDecl from previous labs" in {
    val e1 = N(3)
    val e2 = Binary(Plus, Var("x"), N(1))
    val e3 = iterateStep(Map.empty, Decl(Const, "x", e1, e2)) 
    assert(e3 === (Map.empty, N(4)))
  } 

  "Decl(Var, x, e1, e2)" should "let x be a mutabale variable" in {
    A.reset()
    val e1 = N(3)
    val e2 = Binary(Seq, Assign(Var("x"), N(4.0)), Var("x"))
    val e3 = iterateStep(Map.empty, Decl(Var, "x", e1, e2))
    assert(e3 === (Map(A(1) -> N(4.0)), N(4.0)))
  }

  "Decl" should "not allow Constants to be reassigned" in {
    val e1 = N(3)
    val e2 = Binary(Seq, Assign(Var("x"), N(4.0)), Var("x"))
    val e3 = Decl(Const, "x", e1, e2)
    intercept[StaticTypeError] {
      typeInfer(Map.empty, e3)
    }
  }    
}
