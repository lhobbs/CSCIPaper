import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class ArithmeticSpec extends FlatSpec {

  "Plus" should "add two number values and return a number using small-step semantics" in {
    val e1 = N(1)
    val e2 = N(2)
    val e3 = iterateStep(Map.empty, Binary(Plus, e1, e2))
    assert(e3 === (Map.empty, N(3)))
  }

  "Plus" should "also be able to concatenate strings using small-step semantics" in {
    val e1 = S("Hello")
    val e2 = S("World")
    val e3 = iterateStep(Map.empty, Binary(Plus, e1, e2))
    assert(e3 === (Map.empty, S("HelloWorld")))
  }

  "Minus" should "subtract two number values and return a number using small-step semantics" in {
    val e1 = N(3)
    val e2 = N(1)
    val e3 = iterateStep(Map.empty, Binary(Minus, e1, e2))
    assert(e3 === (Map.empty, N(2)))
  }

  "Times" should "multiply two number values and return a number using small-step semantics" in {
    val e1 = N(3)
    val e2 = N(2)
    val e3 = iterateStep(Map.empty, Binary(Times, e1, e2))
    assert(e3 === (Map.empty, N(6)))
  }

  "Div" should "divide two number values and return a number using small-step semantics" in {
    val e1 = N(8)
    val e2 = N(5)
    val e3 = iterateStep(Map.empty, Binary(Div, e1, e2))
    assert(e3 === (Map.empty, N(1.6)))
  }

}
