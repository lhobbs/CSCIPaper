object Lab3 {
  import jsy.lab3.ast._
  
  /*
   * CSCI 3155: Lab 3 
   * <Lisa Hobbs>
   * 
   * Partner: <Daniel Morrissey>
   * Collaborators: <Josh Rahm>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace 'YourIdentiKey' in the object name above with your IdentiKey.
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
  
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }
  
  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(false) => 0
      case B(true) => 1
      case Undefined => Double.NaN
      case S(s) => try s.toDouble catch { case _ => Double.NaN }
      case Function(_, _, _) => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) if ((n compare 0.0) == 0 || (n compare -0.0) == 0 || n.isNaN()) => false
      case N(_) => true
      case B(b) => b
      case Undefined => false
      case S("") => false
      case S(_) => true
      case Function(_, _, _) => true
    }
  }
  
  def toString(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n.toString
      case B(b) => b.toString
      case Undefined => "undefined"
      case S(s) => s
      case Function(_, _, _) => "function"
    }
  }
  
  /* Big-Step Interpreter with Dynamic Scoping */
  
  /*
   * This code is a reference implementation of JavaScripty without
   * strings and functions (i.e., Lab 2).  You are to welcome to
   * replace it with your code from Lab 2.
   */
  def eval(env: Env, e: Expr): Expr = {
    def eToN(e: Expr): Double = toNumber(eval(env, e))
    def eToB(e: Expr): Boolean = toBoolean(eval(env, e))
    def eToVal(e: Expr): Expr = eval(env, e)
    e match {
      /* Base Cases */
      case _ if (isValue(e)) => e
      case Var(x) => get(env, x)
      
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      
      case Unary(Neg, e1) => N(- eToN(e1))
      case Unary(Not, e1) => B(! eToB(e1))
      
      /* Includes the string cases */
      case Binary(Plus, e1, e2) => (eval(env, e1), eval(env, e2)) match{
        case (S(s1), v2) => S(s1 + toString(v2))
        case (v1, S(s2)) => S(toString(v1) + s2)
        case (v1, v2) => N(toNumber(v1) + toNumber(v2))
      }
        
      case Binary(Minus, e1, e2) => N(eToN(e1) - eToN(e2))
      case Binary(Times, e1, e2) => N(eToN(e1) * eToN(e2))
      case Binary(Div, e1, e2) => N(eToN(e1) / eToN(e2))
      
      /* Function causes error */
      case Binary(Eq, e1, e2) => (eToVal(e1), eToVal(e2)) match{
        case (Function(p, x, ex), e2) => throw new DynamicTypeError(e)
        case (e1, Function(p,x,ex)) => throw new DynamicTypeError(e)
        case _ => B(eToVal(e1) == eToVal(e2))
      } 
        
      case Binary(Ne, e1, e2) => (eToVal(e1), eToVal(e2)) match{
        case (Function(p,x,ex), e2) => throw new DynamicTypeError(e)
        case (e1, Function(p,x,ex)) => throw new DynamicTypeError(e)
        case _ => B(eToVal(e1) != eToVal(e2))
      }
      
      /* Includes string comparisons */
      case Binary(Lt, e1, e2) => (eval(env, e1), eval(env, e2)) match{
        case(S(s1), v2) => B(s1 < toString(v2))
        case (v1, S(s2)) => B(toString(v1) < s2)
        case (v1, v2) => B(eToN(e1) < eToN(e2))
      }
        
      case Binary(Le, e1, e2) => (eval(env, e1), eval(env, e2)) match{
        case(S(s1), v2) => B(s1 <= toString(v2))
        case (v1, S(s2)) => B(toString(v1) <= s2)
        case (v1, v2) => B(eToN(e1) <= eToN(e2))
      }
      
      case Binary(Gt, e1, e2) => (eval(env, e1), eval(env, e2)) match{
        case(S(s1), v2) => B(s1 > toString(v2))
        case (v1, S(s2)) => B(toString(v1) > s2)
        case (v1, v2) => B(eToN(e1) > eToN(e2))
      }
      
      case Binary(Ge, e1, e2) => (eval(env, e1), eval(env, e2)) match{
        case(S(s1), v2) => B(s1 >= toString(v2))
        case (v1, S(s2)) => B(toString(v1) >= s2)
        case (v1, v2) => B(eToN(e1) >= eToN(e2))
      }
      
      case Binary(And, e1, e2) => if (eToB(e1)) eToVal(e2) else B(false)
      case Binary(Or, e1, e2) => if (eToB(e1)) B(true) else eToVal(e2)
      
      case Binary(Seq, e1, e2) => eToVal(e1); eToVal(e2)
      
      case If(e1, e2, e3) => if (eToB(e1)) eToVal(e2) else eToVal(e3)
      
      case ConstDecl(x, e1, e2) => eval(extend(env, x, eToVal(e1)), e2)
      
      case Call(e1, e2) => (eval(env,e1), e2) match{
        case (Function(None, x, e1), e2) => val env2 = extend(env, x, eval(env,e2)); eval(env2, e1) 
        case (Function(Some(x1), x2, ex), e2) =>  val env2 = extend(env, x1, eval(env,e1)); val env3 = extend(env2, x2, eval(env,e2)); eval(env3, ex);
        case _ => throw new DynamicTypeError(e)
      }
        
      case _ => throw new UnsupportedOperationException
    }
  }
    
  def evaluate(e: Expr): Expr = eval(emp, e)
  
  
  /* Small-Step Interpreter with Static Scoping */
  
  def substitute(e: Expr, v: Expr, x: String): Expr = { 
    require(isValue(v))
    /* Simple helper that calls substitute on an expression
     * with the input value v and variable name x. */
    def subst(e: Expr): Expr = substitute(e, v, x)
    /* Body */
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
      case Binary(bop,e1,e2) => Binary(bop, subst(e1), subst(e2))
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Call(e1, e2) => Call(subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      
      case Var(y) if (x==y) => v
      case Var(y) => e
      
      case ConstDecl(y, e1, e2) if (x==y) => ConstDecl(x, subst(e1), e2)
      case ConstDecl(y, e1, e2) => ConstDecl(y,subst(e1),subst(e2))

      case Function(a, x1, e1) if(x == x1) => e
      case Function(Some(x1), x2, e1) if (x1 == x) => e
      case Function(a, x1, e1) => Function(a, x1, subst(e1))

      case _ => throw new DynamicTypeError(e)
    }
  }
    
  def step(e: Expr): Expr = {
    require(!isValue(e))
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if (isValue(v1)) => println(pretty(v1)); Undefined

      /* Do Neg and Not */
      case Unary(uop, e1) if (isValue(e1)) => uop match{
        case Neg => N(-toNumber(e1))
        case Not => B(!toBoolean(e1))
      }
      
      /* Do arithmatic */
      case Binary(Plus, e1, e2) if (isValue(e1) && isValue(e2)) =>  (e1, e2) match{
        case (S(s1), v2) => S(s1 + toString(v2))
        case (v1, S(s2)) => S(toString(v1) + s2)
        case (v1, v2) => N(toNumber(v1) + toNumber(v2))
      } 
      case Binary(Minus, e1, e2) if (isValue(e1) && isValue(e2)) => N(toNumber(e1) - toNumber(e2))
      case Binary(Times, e1, e2) if (isValue(e1) && isValue(e2)) => N(toNumber(e1)*toNumber(e2))
      case Binary(Div, e1, e2) if (isValue(e1) && isValue(e2)) => N(toNumber(e1)/toNumber(e2))
      
      /* Do === and !== */
      case Binary(Eq, e1, e2) if (isValue(e1) && isValue(e2))=> (e1, e2) match{
        case (Function(p, x, ex), e2) => throw new DynamicTypeError(e)
        case (e1, Function(p,x,ex)) => throw new DynamicTypeError(e)
        case _ => B(e1 == e2)
    }
      case Binary(Ne, e1, e2)if (isValue(e1) && isValue(e2)) => (e1, e2) match{
        case (Function(p, x, ex), e2) => throw new DynamicTypeError(e)
        case (e1, Function(p,x,ex)) => throw new DynamicTypeError(e)
        case _ => B(e1 != e2)
    }
      
      /* Do comparisons*/
      case Binary(Lt, e1, e2) if (isValue(e1) && isValue(e2)) => (e1, e2) match{
        case(S(s1), v2) => B(s1 < toString(v2))
        case (v1, S(s2)) => B(toString(v1) < s2)
        case (v1, v2) => B(toNumber(e1) < toNumber(e2))
      }
      case Binary(Le, e1, e2) if (isValue(e1) && isValue(e2)) => (e1, e2) match{
        case(S(s1), v2) => B(s1 <= toString(v2))
        case (v1, S(s2)) => B(toString(v1) <= s2)
        case (v1, v2) => B(toNumber(e1) <= toNumber(e2))
      }
      case Binary(Gt, e1, e2) if (isValue(e1) && isValue(e2)) => (e1, e2) match{
        case (S(s1), v2) => B(s1 > toString(v2))
        case (v1, S(s2)) => B(toString(v1) > s2)
        case (v1, v2) => B(toNumber(e1) > toNumber(e2))
      }
      case Binary(Ge, e1, e2) if (isValue(e1) && isValue(e2)) => (e1, e2) match{
        case(S(s1), v2) => B(s1 >= toString(v2))
        case (v1, S(s2)) => B(toString(v1) >= s2)
        case (v1, v2) => B(toNumber(e1) >= toNumber(e2))
      }
  
      /* Do Binary And, Or and Seq */
      case Binary(And, e1, e2) if (isValue(e1)) => B(toBoolean(e1)) match{
        case B(true) => e2
        case _ => B(false)
      }
      case Binary(Or, e1, e2) if (isValue(e1)) => B(toBoolean(e1)) match{
        case B(true) => B(true)
        case _ => e2
      }
      case Binary(Seq, e1, e2) if (isValue(e1)) => e2
      
      /* Do If */
      case If(e1, e2, e3) if (isValue(e1)) => B(toBoolean(e1)) match{
        case B(true) => e2
        case _ => e3
      }
      
      /* Do ConstDecl */
      case ConstDecl(x, e1, e2) if (isValue(e1)) => substitute(e2, e1, x)  
      
     /* Do Call */
     case Call(e1, e2) if( isValue(e1) && isValue(e2)) => (e1, e2) match{
        case (Function(None, x, e1), e2) => substitute(e1, e2, x)
        case (Function(Some(x1), x2, ex), e2) if isValue(e1) => substitute(substitute(ex, e1, x1), e2, x2)
        case _ => throw new DynamicTypeError(e)
      }
      
      /* Inductive Cases: Search Rules -----------------------------------------------------------------------------------------*/
      case Print(e1) => Print(step(e1))
      
      /* Search Not and Neg */
      case Unary(uop, e1) if !isValue(e1) => uop match{
        case Neg => Unary(Neg, step(e1))
        case Not => Unary(Not, step(e1))
      }

      /* Step through e1 */
      case Binary(Plus, e1, e2) if !isValue(e1) => Binary(Plus, step(e1), e2)
      case Binary(Minus, e1, e2) if !isValue(e1) => Binary(Minus, step(e1), e2)
      case Binary(Times, e1, e2) if !isValue(e1) => Binary(Times, step(e1), e2)
      case Binary(Div, e1, e2) if !isValue(e1) => Binary(Div, step(e1), e2) 
      /* Step through e2 */
      case Binary(Plus, e1, e2) if !isValue(e2) => Binary(Plus, e1, step(e2))      
      case Binary(Minus, e1, e2) if !isValue(e2) => Binary(Minus, e1, step(e2))
      case Binary(Times, e1, e2) if !isValue(e2) => Binary(Times, e1, step(e2))
      case Binary(Div, e1, e2) if !isValue(e2) => Binary(Div, e1, step(e2))
      
      /* Search Binary === and !== */
      case Binary(Eq, e1, e2) if !isValue(e2) => e1 match{
        case Function(p, x, ex) => throw new DynamicTypeError(e)
        case _ => Binary(Eq, e1, step(e2))
      }
      case Binary(Ne, e1, e2) if !isValue(e2) => e1 match{
        case Function(p, x, ex) => throw new DynamicTypeError(e)
        case _ => Binary(Ne, e1, step(e2))
      }

      /* Step through e1 */
      case Binary(Lt, e1, e2) if !isValue(e1) => Binary(Lt, step(e1), e2) 
      case Binary(Le, e1, e2) if !isValue(e1) => Binary(Le, step(e1), e2)
      case Binary(Gt, e1, e2) if !isValue(e1) => Binary(Gt, step(e1), e2)
      case Binary(Ge, e1, e2) if !isValue(e1) => Binary(Ge, step(e1), e2)
      /* Step through e2 */
      case Binary(Lt, e1, e2) if !isValue(e2) => Binary(Lt, e1, step(e2))
      case Binary(Le, e1, e2) if !isValue(e2) => Binary(Le, e1, step(e2))
      case Binary(Gt, e1, e2) if !isValue(e2) => Binary(Gt, e1, step(e2))
      case Binary(Ge, e1, e2) if !isValue(e2) => Binary(Ge, e1, step(e2))

      /* Search Binary And, Or, Seq*/
      case Binary(And, e1, e2) if !isValue(e1) => Binary(And, step(e1), e2)
      case Binary(Or, e1, e2) if !isValue(e1) => Binary(Or, step(e1), e2)
      case Binary(Seq, e1, e2) if !isValue(e1) => Binary(Seq, step(e1), e2)
      
      /* Search If */
      case If(e1, e2, e3) if !isValue(e1) => If(step(e1), e2, e3)
      
      /* Search ConstDecl */
      case ConstDecl(x, e1, e2) if (!isValue(e1)) => ConstDecl(x, step(e1), e2)

      /* Step through e1 */
      case Call(e1, e2) if !isValue(e1) => Call(step(e1), e2)
      /* Step through e2 */
      case Call(e1, e2) if !isValue(e2) => e1 match{
        case Function(_, _, _) => Call(e1, step(e2))
        case _ => throw new DynamicTypeError(e)
      } 
      
      case _ => throw new UnsupportedOperationException
    }
  }
  
  def iterateStep(e: Expr): Expr =
    if (isValue(e)) e else iterateStep(step(e))
    
}
