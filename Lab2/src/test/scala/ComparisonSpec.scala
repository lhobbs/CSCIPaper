import org.scalatest._
import jsy.lab2.ast._
import Lab2._

class ComparisonSpec extends FlatSpec {

  "Eq" should "return true if two numerical values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Eq, e1, e2))
    assert(e3 === B(true))
  } 
  
  "Eq" should "return false if two numerical values are not the same" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = evaluate(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  "Ne" should "return true if two numerical values are different" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = evaluate(Binary(Ne, e1, e2))
    assert(e3 === B(true))
  } 
  
  "Ne" should "return false if two numerical values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Ne, e1, e2))
    assert(e3 === B(false))
  }

  "Lt" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = evaluate(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  } 
  
  "Lt" should "return false if the first expression is not strictly less than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = evaluate(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  } 
  
  "Lt" should "return false if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  } 

  "Le" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = evaluate(Binary(Le, e1, e2))
    assert(e3 === B(true))
  } 
  
  "Le" should "return false if the first expression is greater than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = evaluate(Binary(Le, e1, e2))
    assert(e3 === B(false))
  } 
  
  "Le" should "return true if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Le, e1, e2))
    assert(e3 === B(true))
  } 

  "Gt" should "return true if the first expression is greater than the second" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = evaluate(Binary(Gt, e1, e2))
    assert(e3 === B(true))
  } 
  
  "Gt" should "return false if the first expression is not strictly greater than the second" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = evaluate(Binary(Gt, e1, e2))
    assert(e3 === B(false))
  } 
  
  "Gt" should "return false if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Gt, e1, e2))
    assert(e3 === B(false))
  } 

  "Ge" should "return true if the first expression is greater than the second" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = evaluate(Binary(Ge, e1, e2))
    assert(e3 === B(true))
  } 
  
  "Ge" should "return false if the first expression is less than the second" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = evaluate(Binary(Ge, e1, e2))
    assert(e3 === B(false))
  } 
  
  "Ge" should "return true if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Ge, e1, e2))
    assert(e3 === B(true))
  }

  "Comparisons" should "produce non-intuitive results given the expressions given" in {
    val e1 = N(5)
    val e2 = Undefined
    assert(evaluate(Binary(Eq,e1,e2)) === B(false))
  } 
}
