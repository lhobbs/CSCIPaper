import org.scalatest._
import ProblemSet2._


class ProblemSetSpec extends FlatSpec {

    "join" should "join two lists together on the first element" in {
         assert(join(List((1, "alice"), (2, "bob"), (3, "charles"), (4, "diana")),
                     List((1, "skiing"), (1, "swimming"),
                          (2, "running"), (2, "swimming"),
                          (3, "skiing"), (3, "running"),
                          (4, "swimming")),
                    (x:(Int, String), y:(Int, String)) => x._1 == y._1 ) ==
                List(((1, "alice"), (1, "skiing")), ((1, "alice"), (1, "swimming")),
                     ((2, "bob"), (2, "running")), ((2, "bob"), (2, "swimming")),
                     ((3, "charles"), (3, "skiing")), ((3, "charles"), (3, "running")),
                     ((4, "diana"), (4, "swimming"))))
    }
    "join" should "join two lists together on an arbitrary predicate" in {
          assert(join(List("I", "am", "done", "now"),
                      List("This", "is", "not", "it"),
                      (x:String, y:String) => x.length == y.length) ==
                 List(("am", "is"), ("am", "it"), ("done", "This"), ("now", "not")))
    }


    val prop1 = And(Or(Not(B(false)), B(true)), B(false))
    val prop2 = And(Or(Not(B(false)), Var("x2")), Var("x1"))
    val prop3 = And(Or(Not(Var("foo")), B(true)), B(false))
    val prop4 = And(Or(Not(Var("foo")), B(true)), B(true))

    "eval" should "evaluate a Boolean expression in a given environment" in {
    	  assert(!eval(Map(), prop1))
    	  assert(eval(Map("x1" -> true, "x2" -> true), prop2))
    	  assert(!eval(Map("foo" -> false), prop3))
    	  assert(eval(Map("foo" -> false), prop4))
    }

    "pow" should "produce the nth Cartesian product of a given List" in {
    	  assert(pow(List(true, false), 0) === List(Nil))
    	  assert(pow(List(true, false), 1) === List(List(true), List(false)))
    	  assert(pow(List(false, true), 1) === List(List(false), List(true)), "Maintain ordering")
    	  assert(pow(List(false, true), 2) === 
	         List(List(false, false),
                      List(false, true),
                      List(true, false),
                      List(true, true)))
    	  assert(pow(List(false, true), 3) ===
	         List(List(false, false, false),
                      List(false, false, true),
                      List(false, true, false),
                      List(false, true, true),
                      List(true, false, false),
                      List(true, false, true),
                      List(true, true, false),
                      List(true, true, true)))
     }

    "vars" should "determine the set of variables in the Boolean expression" in {
    	  assert(vars(prop1) === Set())
    	  assert(vars(prop2) === Set("x1", "x2"))
    	  assert(vars(prop3) === Set("foo"))
    	  assert(vars(prop4) === Set("foo"))
    }

    "satisfiable" should "decide if the Boolean expression can be evaluated to true in some environment" in {
    	  assert(!satisfiable(prop1))
    	  assert(satisfiable(prop2))
    	  assert(!satisfiable(prop3))
    	  assert(satisfiable(prop4))
    }

}
