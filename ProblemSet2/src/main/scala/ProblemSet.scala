object ProblemSet2 {

/* Rewrite the below function from for expression syntax to its equivalent using map, flatMap, and withFilter

    def join[A,B](xs: List[A], ys: List[B], p: (A, B) => Boolean):List[(A,B)] = {
        for (x <- xs; y <- ys if p(x, y)) yield (x, y)
    }
*/

    def join[A,B](xs: List[A], ys: List[B], p: (A, B) => Boolean):List[(A,B)] = 
    	xs.flatMap(x => ys.withFilter(y => p(x, y)).map(y => (x,y)))


    type Env = Map[String, Boolean]

    sealed abstract class Expr
    case class B(b: Boolean) extends Expr
    case class Var(x: String) extends Expr
    case class Not(e1: Expr) extends Expr
    case class And(e1: Expr, e2: Expr) extends Expr
    case class Or(e1: Expr, e2: Expr) extends Expr


//Uncomment and fill in the correct construction for E0-E4

    val E0 = And(B(true), (Or(Not(B(false)), Var("x"))))//true && !false | x
    val E1 = And(Var("x1"), And(Var("x2"), Var("x3"))) //e1 && e2 && e3
    val E2 = And(Or(Not(Var("x1")), Or(Not(Var("x2")), Not(Var("x3")))), Or(Var("x1"), Or(Var("x2"), Var("x4"))))//(!x1 | !x2 | !x3) && (x1 | x2 | x4)
    val E3 = Or(Not(Var("x")), Var("y")) //!x | y
    val E4 = And(Var("x"), Or(Var("y"), And(Var("z"), Var("w")))) //x && (y | (z && w))

    def eval(env: Env, e: Expr): Boolean =
      e match{
      case B(b) => b
      case Var(x) => env.get(x).get
      case And(e1, e2) => if (eval(env, e1)) eval(env, e2) else false
      case Or(e1, e2) => if (eval(env, e1)) true else eval(env, e2)
      case Not(e1) => if (eval(env, e1)) false else true
    }

      
    def pow[T](xs: List[T], n: Int): List[List[T]] = {
        def product(xs: List[T], ys: List[List[T]]): List[List[T]] = {
            for (x <- xs;
                 y <- ys) yield x :: y
        }
        n match {
            case 0 => List(Nil)
            case _ => product(xs, pow(xs, n-1))
        }
    }

    def vars(e: Expr): Set[String] = {
      var s : Set[String] = Set()
      e match{
      case Var(x) => s + x
      case And(e1, e2) => s ++ vars(e1) ++ vars(e2)
      case Or(e1, e2) => s ++ vars(e1) ++ vars(e2)
      case Not(e1) => s ++ vars(e1)
      case B(b) => s
    }
    }

    def satisfiable(e: Expr): Boolean = {
        val names = vars(e)
        val possibilities = for (values <- pow(List(false, true), names.size))
            yield Map() ++ (names zip values)
        val sat = for (i <- possibilities) yield eval(i, e)
        sat.contains(true)
    }
}
