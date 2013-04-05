import org.scalatest._
import jsy.lab5.ast._
import Lab5._

class ComparisonSpec extends FlatSpec {

  "Eq" should "return true if two numerical values are not the same using small-step semantics" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = iterateStep(Map.empty, Binary(Eq, e1, e2))
    assert(e3 === (Map.empty, B(true)))
  }
  
  "Eq" should "return false if two numerical values are not the same using small-step semantics" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = iterateStep(Map.empty, Binary(Eq, e1, e2))
    assert(e3 === (Map.empty, B(false)))
  }

  "Ne" should "return true if two numerical values are different using small-step semantics" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = iterateStep(Map.empty, Binary(Ne, e1, e2))
    assert(e3 === (Map.empty, B(true)))
  } 
  
  "Ne" should "return false if two numerical values are the same using small-step semantics" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = iterateStep(Map.empty, Binary(Ne, e1, e2))
    assert(e3 === (Map.empty, B(false)))
  }

  "Lt" should "return true if the first expression is less than the second using small-step semantics" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = iterateStep(Map.empty, Binary(Lt, e1, e2))
    assert(e3 === (Map.empty, B(true)))
  } 
  
  "Lt" should "return false if the first expression is not strictly less than the second using small-step semantics" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = iterateStep(Map.empty, Binary(Lt, e1, e2))
    assert(e3 === (Map.empty, B(false)))
  } 
  
  "Lt" should "return false if two number values are the same using small-step semantics" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = iterateStep(Map.empty, Binary(Lt, e1, e2))
    assert(e3 === (Map.empty, B(false)))
  } 

  "Le" should "return true if the first expression is less than the second using small-step semantics" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = iterateStep(Map.empty, Binary(Le, e1, e2))
    assert(e3 === (Map.empty, B(true)))
  } 
  
  "Le" should "return false if the first expression is greater than the second using small-step semantics" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = iterateStep(Map.empty, Binary(Le, e1, e2))
    assert(e3 === (Map.empty, B(false)))
  } 
  
  "Le" should "return true if two number values are the same using small-step semantics" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = iterateStep(Map.empty, Binary(Le, e1, e2))
    assert(e3 === (Map.empty, B(true)))
  } 

  "Gt" should "return true if the first expression is greater than the second using small-step semantics" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = iterateStep(Map.empty, Binary(Gt, e1, e2))
    assert(e3 === (Map.empty, B(true)))
  } 
  
  "Gt" should "return false if the first expression is not strictly greater than the second using small-step semantics" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = iterateStep(Map.empty, Binary(Gt, e1, e2))
    assert(e3 === (Map.empty, B(false)))
  } 
  
  "Gt" should "return false if two number values are the same using small-step semantics" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = iterateStep(Map.empty, Binary(Gt, e1, e2))
    assert(e3 === (Map.empty, B(false)))
  } 

  "Ge" should "return true if the first expression is greater than the second using small-step semantics" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = iterateStep(Map.empty, Binary(Ge, e1, e2))
    assert(e3 === (Map.empty, B(true)))
  } 
  
  "Ge" should "return false if the first expression is less than the second using small-step semantics" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = iterateStep(Map.empty, Binary(Ge, e1, e2))
    assert(e3 === (Map.empty, B(false)))
  } 
  
  "Ge" should "return true if two number values are the same using small-step semantics" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = iterateStep(Map.empty, Binary(Ge, e1, e2))
    assert(e3 === (Map.empty, B(true)))
  }
}
