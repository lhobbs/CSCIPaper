import org.scalatest._
import ProblemSet1._

class ProblemSet1Spec extends FlatSpec  {
  
  "3 drop[a,b,c,d,e,f,g,h,i,j,k]" should "return [a,b,d,e,g,h,j,k] " in {
    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == 
       List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }
  
  "3 rotate[a,b,c,d,e,f,g,h,i,j,k]" should "return [d,e,f,g,h,i,j,k,a,b,c] " in { 
  assert(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == 
       List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)) 
  }
  
  "-2 rotate[a,b,c,d,e,f,g,h,i,j,k]" should "return [j,k,a,b,c,d,e,f,g,h,i] " in{
    assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == 
       List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }
  
  "p = A == B join[1,2,3], [3,2,1]" should "return [1,1],[2,2],[3,3]" in {
    p = A == B
    assert(join(List(1,2,3), List(3,2,1), p)) == List((1,1),(2,2),(3,3))
  }
  
  

}