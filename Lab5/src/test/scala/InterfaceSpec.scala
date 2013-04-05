import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class InterfaceSpec extends FlatSpec {

  "Interfaces" should "be replaced by their actual types with removeInterfaceDecl " in {
    
    val ftype = Some(TVar("myNumObj"))
    val fbody = Unary(Cast(TVar("myNumObj")), Obj(Map("x" -> N(5.1))))
    val fun = Function(None, List(), ftype, fbody)
    val interface = TInterface("myNumObj", TObj(Map("x" -> TNumber)))

    val results = Function(None, List(), Some(interface), Unary(Cast(interface), Obj(Map("x" -> N(5.1)))))

    val e1 = InterfaceDecl("myNumObj", TObj(Map("x" -> TNumber)), fun)
    val e2 = removeInterfaceDecl(e1)
    assert(e2 == results)
  }
}
