
object Lab2 {
  import jsy.lab2.ast._
  
  /*
   * CSCI 3155: Lab 2
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
      case _ => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case N(n) => n match{
        case 0 => false
        case Double.NaN => false
        case _ => true
      }
      case Undefined => false
      case null => false
      case Var(" ") => false
      case Var("false") => false
      case _ => true
    }
  }
    
  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case _ if (isValue(e)) => e
      
      /* Inductive Cases */
      case Print(e1) => println(eval(env, e1)); Undefined
      
      case N(n) => N(n)
      
      case B(b) => B(b)
      
      case Binary(bop, e1, e2) => bop match{
        case Eq => B(e1 == e2)
        case Ne => B(e1 != e2)
        case Lt => B(toNumber(eval(env,e1)) < toNumber(eval(env,e2)))
        case Le => B(toNumber(eval(env,e1)) <= toNumber(eval(env,e2)))
        case Gt => B(toNumber(eval(env,e1)) > toNumber(eval(env,e2))) 
        case Ge => B(toNumber(eval(env,e1)) >= toNumber(eval(env,e2)))
        case Div => N(toNumber(eval(env, e1)) / toNumber(eval(env,e2)))
        case Times => N(toNumber(eval(env, e1)) * toNumber(eval(env,e2)))
        case Plus => N(toNumber(eval(env, e1)) + toNumber(eval(env,e2)))
        case Minus => N(toNumber(eval(env, e1)) - toNumber(eval(env,e2)))
        case And => B(toBoolean(eval(env,e1))) match {
          case B(true) => B(toBoolean(eval(env,e2)))
          case _ => B(false)
        } 
        case Or => B(toBoolean(eval(env,e1))) match{
          case B(true) => B(true)
          case B(false) => B(toBoolean(eval(env,e2)))
        } 
        case Seq => eval(env, e1); eval(env, e2)
        case _ => Undefined
      } 
      
      case ConstDecl(x, e1, e2) => val evn2 = extend(env, x, eval(env,e1)); eval(evn2, e2)
      
      case If(e1, e2, e3) => B(toBoolean(eval(env, e1))) match{
        case B(true) => eval(env,e2)
        case B(false) => eval(env,e3)
      }
      
      case Unary(uop, e1) => uop match{
        case Neg => N(-toNumber(e1))
        case Not => B(!toBoolean(e1))
      }
      
      case Undefined => Undefined
      
      case Var(s) => get(env, s)
      
      case _ => Undefined
    }
  }
      
  def evaluate(e: Expr): Expr = eval(emp, e)
    
}