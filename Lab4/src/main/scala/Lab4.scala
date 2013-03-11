object Lab4 {
  import jsy.lab4.ast._
  
  /*
   * CSCI 3155: Lab 4
   * <Lisa Hobbs>
   * 
   * Partner: <Daniel Morrissey>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the 'throw new UnsupportedOperationException' expression with
   * your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  /* Collections and Higher-Order Functions */
  
  /* Lists */
  
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => Nil //???
    case h1 :: (t1 @ (h2 :: _)) => h1::compressRec(t1.dropWhile(_==h1))
  }
  
  /*(1,1,1,2,2,3,3) 1::3 --->  (1,2,3)
   * (1,1,1,2,2,3,3,1) --> (1,2,3,1)
   * list (1,1,1,2,2,3,3) --> h 3 --> acc (3) --> return acc
   * list (1,1,1,2,2,3) --> h 3 --> acc (3) --> return acc
   * list (1,1,1,2,2) --> h 2 --> acc (2,3) --> return acc 
   */
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    /* h is element in list
     * acc is accumulator
     * h and acc are parameters we pass to the higher order function 
     * list can be expressed in by h::t, h is head of list, t is tail
     * nill means empty list */
    (h, acc) => acc match{
      case h1::t => if( h == h1 ) acc else h::acc
      case Nil => h::acc
    }
  }
  
  def testCompress(compress: List[Int] => List[Int]): Boolean =
    compress(List(1, 2, 2, 3, 3, 3)) == List(1, 2, 3)
  assert(testCompress(compressRec))
  assert(testCompress(compressFold))
  
  def mapFirst[A](f: A => Option[A])(l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException
    case h :: t => throw new UnsupportedOperationException 
  }
  
  def testMapFirst(mapFirst: (Int => Option[Int]) => List[Int] => List[Int]): Boolean =
    mapFirst((i: Int) => if (i < 0) Some(-i) else None)(List(1, 2, -3, 4, -5)) == List(1, 2, 3, 4, -5)
  //assert(testMapFirst(mapFirst))
  
  /* Trees */
  
  sealed abstract class Tree {
    def insert(n: Int): Tree = this match {
      case Empty => Node(Empty, n, Empty)
      case Node(l, d, r) => if (n < d) Node(l insert n, d, r) else Node(l, d, r insert n)
    } 
    
    def map(f: Int => Int): Tree = this match {
      case Empty => throw new UnsupportedOperationException
      case Node(l, d, r) => throw new UnsupportedOperationException
    }
    
    def foldLeft[A](z: A)(f: (A, Int) => A): A = {
      def loop(acc: A, t: Tree): A = t match {
        case Empty => acc//throw new UnsupportedOperationException
        case Node(l, d, r) => loop(acc,l)//throw new UnsupportedOperationException
      }
      loop(z, this)
    }
    
    def pretty: String = {
      def p(acc: String, t: Tree, indent: Int): String = t match {
        case Empty => acc
        case Node(l, d, r) =>
          val spacer = " " * indent
          p("%s%d%n".format(spacer, d) + p(acc, l, indent + 2), r, indent + 2)
      } 
      p("", this, 0)
    }
  }
  case object Empty extends Tree
  case class Node(l: Tree, d: Int, r: Tree) extends Tree
  
  def treeFromList(l: List[Int]): Tree =
    l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }
  
  def incr(t: Tree): Tree = t.map(i => i + 1)
  //def incr(t: SearchTree): SearchTree = t.map{ i => i + 1 }
  //def incr(t: SearchTree): SearchTree = t.map{ _ + 1 } // using placeholder notation
  
  def testIncr(incr: Tree => Tree): Boolean =
    incr(treeFromList(List(1,2,3))) == treeFromList(List(2,3,4))
  //assert(testIncr(incr))
  
  def sum(t: Tree): Int = t.foldLeft(0){ (acc, d) => acc + d }
  
  def testSum(sum: Tree => Int): Boolean =
    sum(treeFromList(List(1,2,3))) == 6
  //assert(testSum(sum))
  
  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = t.foldLeft((true, None: Option[Int])){
      throw new UnsupportedOperationException
    }
    b
  }
  
  def testStrictlyOrdered(strictlyOrdered: Tree => Boolean): Boolean =
    !strictlyOrdered(treeFromList(List(1,1,2)))
  //assert(testStrictlyOrdered(strictlyOrdered))
  
  /* Type Inference */
  
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case _ => throw new UnsupportedOperationException
  }
  
  def typeInfer(env: Map[String,Typ], e: Expr): Typ = {
    def typ(e1: Expr) = typeInfer(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw new StaticTypeError(tgot, e1, e)
    e match {
      case Print(e1) => typ(e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => env(x)
      case ConstDecl(x, e1, e2) => typeInfer(env + (x -> typ(e1)), e2)
      case Unary(Neg, e1) => typ(e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      
      case Binary(Plus, e1, e2) => (typ(e1),typ(e2)) match{ 
        case (TNumber,TNumber)=>TNumber
        case (TString,TString)=>TString
        case (t1,t2)=>err(t1,e1); err(t2, e2) //need both errors or just one?
      }
      case Binary(Minus, e1, e2) => (typ(e1), typ(e2)) match{
        case (TNumber, TNumber) => TNumber
        case (t1, t2) => err(t1, e1); err(t2, e1)
      }
      case Binary(Times, e1, e2) => (typ(e1), typ(e2)) match{
        case (TNumber, TNumber) => TNumber
        case (t1, t2) => err(t1, e1); err(t2, e2)
      }
      case Binary(Div, e1, e2) => (typ(e1), typ(e2)) match{
        case (TNumber, TNumber) => TNumber
        case (t1, t2) => err(t1, e1); err(t2,e2)
      }
      case Binary(Ge, e1, e2) => (typ(e1), typ(e2)) match{
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (t1, t2) => err(t1, e1)
      }
      case Binary(Gt, e1, e2) => (typ(e1), typ(e2)) match{
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (t1, t2) => err(t1, e1)
      }
      case Binary(Le, e1, e2) => (typ(e1), typ(e2)) match{
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (t1, t2) => err(t1, e1)
      }
      case Binary(Lt, e1, e2) => (typ(e1), typ(e2)) match{
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (t1, t2) => err(t1, e1)
      }
      case Binary(Eq, e1, e2) => (typ(e1), typ(e2)) match{
        case (TFunction(p1, p2), t2) => err(TFunction(p1, p2), e1)
        case (t1, TFunction(p1, p2)) => err(t1, e1)
        case (t1, t2) => TBool
      }
      case Binary(Ne, e1, e2) => (typ(e1), typ(e2)) match{
        case (TFunction(p1, p2), t2) => err(TFunction(p1, p2), e1)
        case (t1, TFunction(p1, p2)) => err(t1, e1)
        case (t1, t2) => TBool
      }
      case Binary(And, e1, e2) => (typ(e1), typ(e2)) match{
        case (TBool, TBool) => TBool
        case (t1, t2) => err(t1, e1)
      }
      case Binary(Or, e1, e2) => (typ(e1), typ(e2)) match{
        case (TBool, TBool) => TBool
        case (t1, t2) => err(t1, e1)
      }
      case Binary(Seq, e1, e2) => (typ(e1), typ(e2)) match{
        case (t1, t2) => t2
      }
      case If(e1, e2, e3) => (typ(e1), typ(e2), typ(e3)) match{
        case (TBool, t2, t3) => if( t2 == t3) t2 else err(t2, e2)
        case (t1, t2, t3) => err(t1, e1)
      }
      
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(rt)) =>
            val tprime = TFunction(params, rt)
            env + (f -> tprime)
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = throw new UnsupportedOperationException
        // Match on whether the return type is specified.
        tann match {
          case None => throw new UnsupportedOperationException
          case Some(rt) => throw new UnsupportedOperationException
        }
      }
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def inferType(e: Expr): Typ = typeInfer(Map.empty, e)
  
  /* Small-Step Interpreter */
  
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    ((v1, v2): @unchecked) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case (N(n1), N(n2)) =>
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }
  
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    
    def subst(e: Expr): Expr = substitute(e, v, x)
    
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Call(e1, args) => Call(subst(e1), args map subst)
      case Var(y) => if (x == y) v else e
      case ConstDecl(y, e1, e2) =>
        ConstDecl(y, subst(e1), if (x == y) e2 else subst(e2))
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def step(e: Expr): Expr = {
    require(!isValue(e))
    
    def stepIfNotValue(e: Expr): Option[Expr] = if (isValue(e)) None else Some(step(e))
    
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      case Unary(Neg, N(n1)) => N(- n1)
      case Unary(Not, B(b1)) => B(! b1)
      case Binary(Seq, v1, e2) if isValue(v1) => e2
      case Binary(Plus, S(s1), S(s2)) => S(s1 + s2)
      case Binary(Plus, N(n1), N(n2)) => N(n1 + n2)
      case Binary(Minus, N(n1), N(n2)) => N(n1 - n2)
      case Binary(Times, N(n1), N(n2)) => N(n1*n2)
      case Binary(Div, N(n1), N(n2)) => N(n1/n2)
      case Binary(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => B(inequalityVal(bop, v1, v2))
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => B(v1 == v2)
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => B(v1 != v2)
      case Binary(And, B(b1), e2) => if (b1) e2 else B(false)
      case Binary(Or, B(b1), e2) => if (b1) B(true) else e2
      case ConstDecl(x, v1, e2) if isValue(v1) => substitute(e2, v1, x)
      case If(B(e1), e2, e3) => if (e1) e2 else e3
      /*** Fill-in more cases here. ***/
        
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      case Unary(uop, e1) => Unary(uop, step(e1))
      case Binary(bop, v1, e2) if isValue(v1) => Binary(bop, v1, step(e2))
      case Binary(bop, e1, e2) => Binary(bop, step(e1), e2)
      case If(e1, e2, e3) => If(step(e1), e2, e3)
      case ConstDecl(x, e1, e2) => ConstDecl(x, step(e1), e2)
      case Call(e1,e2) => Call(step(e1), e2)
      /*** Fill-in more cases here. ***/
      
      /* Everything else is a stuck error. */
      case _ => throw new StuckError(e)
    }
  }

  def iterateStep(e: Expr): Expr =
    if (isValue(e)) e else iterateStep(step(e))
    
}
