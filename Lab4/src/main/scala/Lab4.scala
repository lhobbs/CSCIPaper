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
    case Nil | _ :: Nil => l //???
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
    case Nil => l
    case h :: t => f(h) match {
      case Some(a) => a::t
      case _ => h::mapFirst(f)(t)
    } 
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
      case Empty => Empty
      case Node(l, d, r) => { Node(l.map(f), f(d), r.map(f))}
    }
    
    def foldLeft[A](z: A)(f: (A, Int) => A): A = {
      def loop(acc: A, t: Tree): A = t match {
        case Empty => acc 
        case Node(l, d, r) => { loop(f(acc,d), l); loop(f(acc,d),r)}
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
  assert(testIncr(incr))
  
  def sum(t: Tree): Int = t.foldLeft(0){ (acc, d) => acc + d }
  
  def testSum(sum: Tree => Int): Boolean =
    sum(treeFromList(List(1,2,3))) == 6
  assert(testSum(sum))
  
  /*def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = t.foldLeft((true, None: Option[Int])){
       t match{
        case Empty => (acc, d) => (true, None)
        case Node(l,d,r) =>  if(strictlyOrdered(l)){ val a2 = (strictlyOrdered(r),None); (a2, d) => a2 } else { val a3 = (false,None); (a3, d) => a3 } //Nope :(
        case _ => (acc, d) => acc
      }
    }
    b
  }*/
  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = t.foldLeft((true, None: Option[Int])){
       t match{
        case Empty => (acc, d) => (true, None)
        case Node(l,d,r) =>  {
            var ret = (true, None:Option[Int]);
            l match {
                case Node(x, y, z) => { 
                    r match {
                        case Node(a, b, c) => if(y >= d || d >= b) ret = (false, None);
                        case Empty => if(y >= d) ret = (false, None);
                    }
                }
                case Empty => r match {
                    case Node(a, b, c) => if (d >= b) ret = (false, None);
                    case Empty => ret = (true, None);
                }
            }
            
            ret = (ret._1 && strictlyOrdered(l) && strictlyOrdered(r), None);
            
           (a, d) => a;
        }
      }
    }
    b
  }
  
  def testStrictlyOrdered(strictlyOrdered: Tree => Boolean): Boolean =
    !strictlyOrdered(treeFromList(List(1,1,2)))
  //assert(testStrictlyOrdered(strictlyOrdered))
  
  /* Type Inference */
  
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(p, z) => true
    case _ => false
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
      case Unary(Not, e1) => typ(e1) match{
        case TBool => TBool
        case t1 => err(t1, e1)
      }
      case Binary(Plus, e1, e2) => (typ(e1),typ(e2)) match{ 
        case (TNumber,TNumber)=>TNumber
        case (TString,TString)=>TString
        case (t1,t2)=> err(t1,e1)
      }
      case Binary(Minus, e1, e2) => (typ(e1), typ(e2)) match{
        case (TNumber, TNumber) => TNumber
        case (t1, t2) => err(t1, e1)
      }
      case Binary(Times, e1, e2) => (typ(e1), typ(e2)) match{
        case (TNumber, TNumber) => TNumber
        case (t1, t2) => err(t1, e1)
      }
      case Binary(Div, e1, e2) => (typ(e1), typ(e2)) match{
        case (TNumber, TNumber) => TNumber
        case (t1, t2) => err(t1, e1)
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
      case Binary(Eq, e1, e2) => (e1, e2) match{
        case (Function(p, params, tann, ex), e2) => err(typ(e1), e1)
        case (e1, Function(p, params, tann, ex)) => err(typ(e2), e2)
        case (e1, e2) => if(typ(e1) == typ(e2)) TBool else err(typ(e1), e1)
      }
        /*(typ(e1), typ(e2)) match {
        case (t1, t2) => 
          if (hasFunctionTyp(t1)) err(t1, e1)
          else if (hasFunctionTyp(t2)) err(t2, e2)
          else if (t1 == t2) TBool
          else err(t1, e1)
      }*/
        
        /*if (hasFunctionTyp(typ(e1))) err(typ(e1), e1) 
      else if (hasFunctionTyp(typ(e2))) err(typ(e2), e2) 
	      else {(typ(e1), typ(e2)) match{
	        case (TFunction(p1, p2), t2) => err(TFunction(p1, p2), e1)
	        case (t1, TFunction(p1, p2)) => errTFunction(p1, p2), e2)
	        case (t1, t2) => if (t1 == t2) TBool else err(t1, e1)
	      }
      }*/
      case Binary(Ne, e1, e2) => (e1, e2) match{
        case (Function(p, params, tann, ex), e2) => err(typ(e1), e1)
        case (e1, Function(p, params, tann, ex)) => err(typ(e2), e2)
        case (e1, e2) => if(typ(e1) == typ(e2)) TBool else err(typ(e1), e1)
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
      case Obj(m1) => { 
    	  val m2 = for((x, ex) <- m1) yield x -> typ(ex);
    	  TObj(m2)
      }
      /*case GetField(e1, f) => typ(e1) match{
        case TObj(fields) => fields.getOrElse(f,err(TObj(fields),e1))
        case t1 => err(t1,e1)
        //case t1 => t1
      }*/
      case GetField(e1, f) =>  typ(e1) match{
        case TObj(fields) => { val fields2 = fields.getOrElse(f, err(typ(e1), e1)); fields2}
        case t1 =>  TUndefined// err(t1, e1)
      }
      /*case Call(e1, args) => typ(e1) match{
        case TFunction(params, tann) => {
            if (params.length != args.length) err (typ(e1), e1)
             else {
               val pl = params zip args
               def typcheck(acc: Typ, pl: ((String, Typ), Expr)): Typ = 
	              pl match{
		              case ((pn, pt), a) => 
		                if (pt != typ(a)) err(typ(e1), e1)
		                else tann
	            } 
            	pl.foldLeft(typ(e1))(typcheck)      
              }
            }
        
        case t => t
      }*/
      case Call(e1, args) => typ(e1) match{
          case TFunction(params, rt) => println("Hello from Call"); if (params.length != args.length) { err(typ(e1), e1)}
            else{
              val p2 = for (i <- params) yield i._2
              val a2 = for (j <- args) yield typ(j)
              val pa = p2 zip a2
              if (pa forall (x => x._1 == x._2)) { TFunction(params,typ(e1)) }
              else { err(typ(e1), e1)}
            }
            
          case TObj(fields) => TObj(fields)
          case _ =>err(typ(e1), e1)//println(typ(e1)); TUndefined//
        
      }
        
        /*e1 match{
            case Function(p, params, tann, e1) => println("HELLO FROM Call Function")  
            if (params.length != args.length) {println("HELLO FROM not same length"); err(typ(e1), e1)}
            else{
              val p2 = for (i <- params) yield i._2
              val a2 = for (j <- args) yield typ(j)
              val pa = p2 zip a2
              if (pa forall (x => x._1 == x._2)) {println("HELLO FROM same typs"); typ(e1) }
              else {println("HELLO FROM not same typs"); err(typ(e1), e1)}
            	  
            }
              /*tann match{
              case _ => 
            
            {
              val params2 = for(i <- params) yield i._2
            if (params2.length != args.length) err(typ(e1), e1)
            
             else{ val pl = params zip args
              def typcheck(acc: Typ, pl: ((String, Typ), Expr)): Typ = 
	              pl match{
		              case ((pn, pt), a) => 
		                if (pt == typ(a)) {
		                  tann match{
		                    case None => TUndefined
		                    case Some(t) => t
		                }
		                  }
		                else err(typ(e1), e1)
	            } 
            	pl match{
            	  case Nil => tann match{
		                    case None => TUndefined
		                    case Some(t) => t
		                }
            	  case _ => pl.foldLeft(typ(e1))(typcheck)
            	}      

             }
            
            }
            }*/
            
            case Var(x) =>  println("HELLO FROM Call Var"); typeInfer(env, e1)// I don't know what to do in this case
            case _ => println("HELLO FROM Call other"); err(typ(e1), e1)
        }*/
      /*case Call(e1, args) => e1 match{
        case Function(p, params, tann, e1) if(params.length == args.length)=> tann match{
          case None => TUndefined
          case Some(t) => t
        }
        case _ => err(typ(e1), e1)
      }*/
    
      case Function(p, params, tann, e1) =>      
        { 
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
        val env2 = params match{
          case Nil => env1
          case h::t => {
            def enviro2 (acc: Map[String, Typ], params: (String, Typ)): Map[String, Typ] =
              params match{
              case (s, t) => env1 + (s -> t)
            }
            params.foldLeft(env)(enviro2)
          }
          case _ =>  err(TUndefined, e1)
        }
        //val env2 = err(TUndefined, e1)
       
        // Match on whether the return type is specified.
        tann match {
          case None =>  typeInfer(env2,e1)//TFunction(params, typeInfer(env2, e1)) //err(typ(e1), e1)
          case Some(rt) =>  rt//TFunction(params, rt)//if (rt == typ(e1)) TFunction(params, rt) else err(typ(e1), e1)
          /*case Some(rt) => {
            if (rt == typ(e1)) TFunction(params, rt)
            else err(typ(e1), e1)
          }*/
          //case Some(rt) => typeInfer(env2, e1)
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
      case Function(Some(x1), params, tann, e1) if (x1 == x) => e
      case Function(p, params, tann, e1) => if(params forall (s => s._1 != x)) Function(p, params, tann, subst(e1)) else e
      //case Function(p, params, tann, e1) if (x == params(0)._1 ) => e1 // checks if x == first name in params
      //case Function(p, params, tann, e1) => Function(p, params, tann, subst(e1))
      case Obj(fields) =>
        val f2 = for (i <- fields) yield i._1 -> subst(i._2);
        Obj(f2)
      
      case GetField(e1, f) => GetField(subst(e1), f)
      //case _ => throw new UnsupportedOperationException
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
      case Call(e1, args) if isValue(e1) && (args forall isValue) => e1 match{
        case Function(None, params, tann, ex) => {
          val pa = params zip args;
          def sub(acc: Expr, pa:((String, Typ), Expr)): Expr = 
            pa match{
            case ((s, t), a) => substitute(acc, a, s)
          }
          val ex2 = pa.foldLeft(ex)(sub)
          ex2
        }
        
        case Function(Some(f), params, tann, ex) => {
          val pa = params zip args;
          def sub(acc: Expr, pa:((String, Typ), Expr)): Expr = 
            pa match{
            case ((s, t), a) => substitute(substitute(acc,e1,f), a, s)
          }
          val ex2 = pa.foldLeft(ex)(sub)
          ex2
        }
        
        case _ => throw new DynamicTypeError(e)
      }
      case GetField(e1, f) if isValue(e1) => e1 match{
        case Obj(fields) => fields.getOrElse(f, throw new StuckError(e))
        case _ => throw new StuckError(e)
      } 
      
      /*** Fill-in more cases here. ***/
        
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      case Unary(uop, e1) => Unary(uop, step(e1))
      case Binary(bop, e1, e2) if !isValue(e1) => Binary(bop, step(e1), e2)
      case Binary(bop, v1, e2) if !isValue(e2) => Binary(bop, v1, step(e2))
      case If(e1, e2, e3) if !isValue(e1) => If(step(e1), e2, e3)
      case ConstDecl(x, e1, e2) => ConstDecl(x, step(e1), e2)
      case Call(e1, args) if !isValue(e1) => Call(step(e1), args)
      /*case Call(e1, args) if !(args forall isValue) => {
        def stp(acc: Expr, args: Expr): Expr ={
          args match{
            case ex if (!isValue(ex)) => step(ex)
            case ex => ex
          }
        
      }
        args.foldLeft(e1)(stp)
      }*/
      case Call(e1, args) if !(args forall isValue) => {
        /*val args2 = mapFirst((ex: Expr) => if (!isValue(ex)) Some(step(ex)) else None)(args)
        Call(e1, args2)*/
        val args2 = for (i <- args) yield step(i)
        Call(e1, args2)
      }
      case GetField(e1, f) => GetField(step(e1), f)
      case Obj(fields) =>
        val x = for (i <- fields) yield i._1 -> step(i._2);
        Obj(x)
      /*case Obj(fields) => {
        val f2 = for ((s, ex) <- fields) yield ex
        val f3 = f2.find((ex: Expr) => !isValue(ex))
        f3 match{
          case None => Obj(fields)
          case Some(se) => step(se)
        }
        val fields2 = for (i <- fields) yield i._1 -> i._2
        Obj(fields2)
      }*/
      /*case Obj(fields) => {
        //val fe: List[Expr] = for (i <- List.range(0, fields.length)) yield i._2
        // fields2 = mapFirst((m: Map[String, Expr]) => 
          //if (!isValue(m)) Some(m) else None)(fe)
        //val f2 = for (i <- fields) yield i._1 -> step(i._2)
        val fs = for (i <- fields) yield i._1
        val fs2 = fs.toList
        val fe = for (i <- fields) yield i._2
        val fe2 = fe.toList
        val f2 = mapFirst((m: Expr ) => if (!isValue(m)) Some(step(m)) else None)(fe2)
        //val f3 = for (i <- fs2; j <- fe2) yield Map(i -> j)
        //val f3 = fs2 zip fe2
        //val f4 = for(j <- List.range(0, f3.length)) yield Map(f3(j)._1 -> f3(j)._2)
        //Obj(f3)
        val obj2 = Obj(Map(fs2(0) -> f2(0), fs2(1) -> fe2(1)))
        obj2
      }*/
      
      /*** Fill-in more cases here. ***/
      
      //case Var(x) => throw new DynamicTypeError(e) //Just so we know if this is trying to go through step
      /* Everything else is a stuck error. */
      case _ => throw new StuckError(e)
      
    }
  }

  def iterateStep(e: Expr): Expr =
    if (isValue(e)) e else iterateStep(step(e))
    
}
