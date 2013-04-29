import org.scalatest._
import jsy.lab2.ast._
import Lab2._

class ArithmeticSpec extends FlatSpec {

  "Plus" should "add two number values and return a number" in {
    val e1 = N(1)
    val e2 = N(2)
    val e3 = evaluate(Binary(Plus, e1, e2))
    assert(e3 === N(3))
  }

  "Minus" should "subtract two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(1)
    val e3 = evaluate(Binary(Minus, e1, e2))
    assert(e3 === N(2))
  }

  "Times" should "multiply two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(2)
    val e3 = evaluate(Binary(Times, e1, e2))
    assert(e3 === N(6))
  }

  "Div" should "divide two number values and return a number" in {
    val e1 = N(8)
    val e2 = N(5)
    val e3 = evaluate(Binary(Div, e1, e2))
    assert(e3 === N(1.6))
  }

  "Arithmetic Operators" should "produce non-intuitive solutions given differing expression types" in {
    val e1 = B(true)
    val e2 = N(7)
    assert(evaluate(Binary(Plus,e1,e2)) == N(8))
  }

}
