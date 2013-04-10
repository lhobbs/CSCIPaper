import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class PassBySpec extends FlatSpec {
  "Pass by Const" should "replace the parameter with a value" in {
    A.reset()
    val fun = Function(None, List(("x", (PConst, TNumber))), None, Var("x"))
    val e1 = Decl(Const, "x", N(4.0), Call(fun, List(Var("x"))))
    assert(iterateStep(Map.empty, e1) === (Map.empty, N(4.0)))
  }

  "Pass by Var" should "replace the parameter with a value" in {
    A.reset()
    val fun = Function(None, List(("x", (PVar, TNumber))), None, Var("x"))
    val e1 = Decl(Var, "x", N(4.0), Call(fun, List(Var("x"))))
    assert(iterateStep(Map.empty, e1) === (Map(A(1) -> N(4.0), A(2) -> N(4.0)), N(4.0)))
  }

  "Pass by Name" should "replace the parameter with an dereference" in {
    A.reset()
    val fun = Function(None, List(("x", (PName, TNumber))), None, Var("x"))
    val e1 = Decl(Var, "x", N(4.0), Call(fun, List(Var("x"))))
    assert(iterateStep(Map.empty, e1) === (Map(A(1) -> N(4.0)), N(4.0)))
  }

  "Pass by Ref" should "replace the parameter with a dereference" in {
    A.reset()
    val fbody = Binary(Seq, Assign(Var("x"), N(5.0)), Var("x"))
    val fun = Function(None, List(("x", (PRef, TNumber))), None, fbody)
    val e1 = Decl(Var, "x", N(4.0), Call(fun, List(Var("x"))))
    assert(iterateStep(Map.empty, e1) === (Map(A(1) -> N(5.0)), N(5.0)))
  }
}
