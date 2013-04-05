import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class CastSpec extends FlatSpec {

  "Cast" should "enable upcasting " in {
    val t1 = TObj(Map("x" -> TNumber))
    val t2 = TObj(Map("x" -> TNumber, "y" -> TBool))
    assert(castOk(t1, t2))
  }

  "Cast" should "enable downcasting" in {
    val t1 = TObj(Map("x" -> TNumber, "y" -> TBool))
    val t2 = TObj(Map("x" -> TNumber))
    assert(castOk(t1, t2))
  }

  "Cast" should "not allow casting between two different types" in {
    val t1 = TObj(Map("x" -> TNumber))
    val t2 = TObj(Map("x" -> TBool))
    assert(!castOk(t1, t2))
  }

  "Cast" should "not allow casting between two different objects" in {
    val t1 = TObj(Map("x" -> TNumber))
    val t2 = TObj(Map("y" -> TBool))
    assert(!castOk(t1, t2))
  }
}
