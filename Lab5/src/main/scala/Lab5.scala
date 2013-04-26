object Lab5 {
  import jsy.lab5.ast._
  
  /*
   * CSCI 3155: Lab 5
   * <Lisa Hobbs>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Daniel Morrissey>
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
  


  /*** Lowering: Remove InterfaceDecl ***/

  def removeInterfaceDecl(e: Expr): Expr = {
    def loop(env: Map[String, Typ], e: Expr): Expr = {
      def tyrm(t: Typ): Typ = t match {
        case TVar(tvar) => env.getOrElse(tvar, throw new IllegalArgumentException("Unknown type name %s.".format(tvar)))
        case TNumber | TBool | TString | TUndefined | TNull => t
        case TFunction(params, rt) =>
          TFunction(params map { case (x,(mode,t)) => (x,(mode,tyrm(t)))}, tyrm(rt))
        case TObj(fields) => TObj(fields map { case (f,t) => (f, tyrm(t)) })
        /* Should never match because introduced in this pass. */
        case TInterface(_, _) => throw new IllegalArgumentException("Gremlins: Encountered TInterface in removeInterfaceDecl.")
      }
      def rm(e: Expr): Expr = loop(env, e)
      val r =
        e match {
        case Unary(Cast(t), e1) => t match{ 
            case TObj(fields) => e1 match {
              case Null =>  Unary(Cast(tyrm(t)),Null) 
              case _ => Unary(Cast(tyrm(t)), rm(e1))

            }         
            case _ => Unary(Cast(tyrm(t)), rm(e1))
          }

          case Function(p, params, retty, e1) => 
            {
              val y = for ((st, (p, ty)) <- params) yield (st, (p, tyrm(ty)))
            val x = retty match {
              case Some(typ) => {
            	  Some(tyrm(typ))
              }
              case _ => retty;
            }
            Function(p, y, x, rm(e1))
            }

          case InterfaceDecl(tvar, t, e1) => 
            {
            val env2 = env + (tvar -> TInterface(tvar, t))
            loop(env2, e1)
          }

          /* Pass through cases. */
          case Var(_) | N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
          case Print(e1) => Print(rm(e1))
          case Unary(uop, e1) => Unary(uop, rm(e1))
          case Binary(bop, e1, e2) => Binary(bop, rm(e1), rm(e2))
          case If(e1, e2, e3) => If(rm(e1), rm(e2), rm(e3))
          case Decl(mut, y, e1, e2) => Decl(mut, y, rm(e1), rm(e2))
          case Call(e1, args) => Call(rm(e1), args map rm)
          case Obj(fields) => Obj(fields map { case (f, e) => (f, rm(e)) })
          case GetField(e1, f) => GetField(rm(e1), f)
          case Assign(e1, e2) => Assign(rm(e1), rm(e2))
        }
      /* Patch up positions for error messages. */
      e match {
        case InterfaceDecl(_, _, _) => r
        case _ => r setPos e.pos
      }
    }
    loop(Map.empty, e)
  }

  /*** Casting ***/

  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
    case (TNull, TObj(_)) => true
    case (_, _) if (t1 == t2) => true
    case (TObj(fields1), TObj(fields2)) =>{ //think I fixed this
      val zipfield = fields1.toList zip fields2.toList; 
      def check(list: List[((String, Typ), (String, Typ))]): Boolean = 
      list match{
        case Nil => true
        case h::t => h match{
          case (f1, f2) => if (f1 == f2) check(t) else false
        }
      }
      check(zipfield)
    } 
    case (_, TInterface(tvar, t2p)) => castOk(t1, typSubstitute(t2p, t2, tvar))
    case (TInterface(tvar, t1p), _) => castOk(typSubstitute(t1p, t1, tvar), t2)
    case _ => false
  }
  
  /*** Type Inference ***/
  
  def typeInfer(env: Map[String,(Mutability,Typ)], e: Expr): Typ = {
    def typ(e1: Expr) = typeInfer(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw new StaticTypeError(tgot, e1, e)
    
    def hasFunctionTyp(t: Typ): Boolean = t match {
      case TFunction(_, _) => true
      case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
      case _ => false
    } 
    def mutOfMode(m: PMode): Mutability = m match {
      case PConst | PName => Const
      case PVar | PRef => Var
    }

    e match {
      case Print(e1) => typ(e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) =>
        val (_, t) = env(x)
        t
      case Unary(Neg, e1) => typ(e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case Unary(Not, e1) => typ(e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      case Binary(Plus, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case TString => typ(e2) match {
          case TString => TString
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Minus|Times|Div, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Eq|Ne, e1, e2) => typ(e1) match {
        case t1 if !hasFunctionTyp(t1) => typ(e2) match {
          case t2 if (t1 == t2) => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TBool
          case tgot => err(tgot, e2)
        }
        case TString => typ(e2) match {
          case TString => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(And|Or, e1, e2) => typ(e1) match {
        case TBool => typ(e2) match {
          case TBool => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Seq, e1, e2) => typ(e1); typ(e2)
      case If(e1, e2, e3) => typ(e1) match {
        case TBool =>
          val (t2, t3) = (typ(e2), typ(e3))
          if (t2 == t3) t2 else err(t3, e3)
        case tgot => err(tgot, e1)
      }
      case Obj(fields) => TObj(fields map { case (f,t) => (f, typ(t)) })
      
      case GetField(e1, f) => typ(e1) match {
        case TObj(tfields) if (tfields.contains(f)) => tfields(f)
        case tgot => err(tgot, e1)
      } 
      
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(rt)) =>
            val tprime = TFunction(params, rt)
            env + (f -> (Const, tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with the parameters.
        val env2 = params match{ 
          case Nil => env1
          case h::t => {
            def enviro2 (acc: Map[String, (Mutability, Typ)], params: (String, (PMode,Typ))): Map[String, (Mutability, Typ)] =
              params match{
              case (s, (p, t)) => acc + (s -> (mutOfMode(p), t))
            }
             params.foldLeft(env1)(enviro2)
          }
          case _ => err(TUndefined, e1)
        }
        e1 match {
          case GetField(Unary(Cast(t), ex1), ex2) => 
            val x = for((s, (p, ty)) <- params; if(ty == TInterface(s, t))) yield (s, (p, ty))
            if (x == 0) err(t, ex1)
          case _ => e1
        }
        // Infer the type of the function body
        val t1 = typeInfer(env2, e1)
        tann foreach { rt => if (rt != t1) err(t1, e1) };
        TFunction(params, t1)
      }
      
      /*** Fill-in more cases here. ***/

      case Null => TNull
      
      case Decl(Var, x, e1, e2) => {
        val env1 = env + (x -> (Var, typ(e1)))
        val t = typeInfer(env1, e2)
        t
      }
      case Decl(Const, x, e1, e2) =>  
          val env1 = env + (x -> (Const, typ(e1)))
          val t = typeInfer(env1, e2)
        	 t
          
      case Assign(e1, e2) =>  e1 match {
        case Var(x) => { 
          env.get(x) match {
            case Some((mut, ty)) => if (mut == Const) err(typ(e1), e1) else typ(e2)
            case None => typ(e2)
          }
          }
        case GetField(e1, f) => typ(e2)
        case _ => err(typ(e2), e2)
      }
        
       
      case Call(e1, args) => 
        typ(e1) match{ 
          case TFunction(params, rt) => if (params.length != args.length)  { err(typ(e1), e1)}
            else{
              val p2 = params.unzip._2
              val p3 = p2.unzip._2
              val pm = p2.unzip._1
              val a2 = args map typ
              val pr = for(p <- pm; a <- args; if (p == PRef)) yield (p, a)
              val pra = for (p <- pm; a <- args; if ((p == PRef) && isLExpr(a))) yield(p, a)
              if (pr != pra) err(typ(e1), e1)
              val pca = for(p <- pm; a <- args; if ((p == PConst) && isLValue(a))) yield(p, a)
              if (a2 == p3) { rt }
              else {err(typ(e1), e1)}
            }  
          case _ =>  err(typ(e1), e1)
      } 
        
      case Unary(Cast(t), e1) => t 
      
      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }
  
  def inferType(e: Expr): Typ = typeInfer(Map.empty, e)
  
  
  /*** Small-Step Interpreter ***/
  
  type Mem = Map[A, Expr]

  /* Do the operation for an inequality. */
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

  /* Rename bound variables in e to avoid capturing free variables in esub. */
  def avoidCapture(esub: Expr, e: Expr): Expr = {
    val avoidVars: Set[String] = freeVars(esub)
    def renameVar(x: String): String = if (avoidVars.contains(x)) renameVar(x + "$") else x
    
    def rename(env: Map[String,String], e: Expr): Expr = {
      def ren(e: Expr): Expr = rename(env, e)
      e match {
        case Var(y) => e
        case Decl(mut, y, e1, e2) => Decl(mut, y, ren(e1), ren(e2)) //??
        case Function(p, params, retty, e1) =>
          val (env1, prenamed) = p match {
            case None => (env, None)
            case Some(y) =>
              val yrenamed = renameVar(y)
              (env + (y -> yrenamed), Some(yrenamed))
          }
          val (env2, revparamsrenamed) = params.foldLeft((env1, Nil: List[(String, (PMode, Typ))])) {
            case ((envacc, renamedacc), (y, modet)) =>
              val yrenamed = renameVar(y)
              (envacc + (y -> yrenamed), (yrenamed, modet) :: renamedacc)
          }
          Function(prenamed, revparamsrenamed.reverse, retty, rename(env2, e1))
        
        /* Pass through cases. */
        case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
        case Unary(uop, e1) => Unary(uop, ren(e1))
        case Binary(bop, e1, e2) => Binary(bop, ren(e1), ren(e2))
        case If(e1, e2, e3) => If(ren(e1), ren(e2), ren(e3))
        case Call(e1, args) => Call(ren(e1), args map ren)
        case Obj(fields) => Obj(fields map { case (f,e) => (f, ren(e)) })
        case GetField(e1, f) => GetField(ren(e1), f)
        case Assign(e1, e2) => Assign(ren(e1), ren(e2))
        case Print(e1) => Print(ren(e1))
        
        /* Should not match: should have been removed. */
        case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected    expression %s.".format(e))
        
      }
    }
    
    rename(Map.empty, e)
  }
  
  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = substitute(e, esub, x)
    avoidCapture(esub, e) match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (x == y) esub else e
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
      case Function(p, params, retty, e1) =>
        if (p == Some(x) || (params exists { case (y,_) => x == y })) e else Function(p, params, retty, subst(e1))
      case Call(e1, args) => Call(subst(e1), args map subst)
      case Obj(fields) => Obj(fields map { case (f,e) => (f, subst(e)) })
      case GetField(e1, f) => GetField(subst(e1), f)
      case Assign(e1, e2) => Assign(subst(e1), subst(e2))
      case InterfaceDecl(tvar, t, e1) => InterfaceDecl(tvar, t, subst(e1))
    }
  }
  
  def step(m: Mem, e: Expr): (Mem, Expr) = {
    require(!isValue(e))
    
    /*** Helpers for Call ***/
    
    /* Return the list with a change for the first place where f returns Some. */
    def mapFirstWith[A,B](b: B, f: ((B,A)) => Option[(B,A)])(l: List[A]): (B,List[A]) = l match {
      case Nil => (b, Nil)
      case h :: t => f(b, h) match {
        case None =>
          val (b1, t1) = mapFirstWith(b, f)(t)
          (b1, h ::t1)
        case Some((b1, h1)) => (b1, h1 :: t)
      }
    }

    /* Check whether or not the argument expression is ready to be applied. */
    def argApplyable(mode: PMode, arg: Expr): Boolean = mode match { //I understand, I think
      case PConst | PVar => isValue(arg)
      case PName => true
      case PRef => isLValue(arg)
    }

    /* Get the memory and argument expression based on the parameter mode. Expr arg should "applyable". */
    def argByMode(mode: PMode, m: Mem, arg: Expr): (Mem, Expr) = mode match {
      case PConst if (isValue(arg)) => (m, arg) //doesn't change
      case PName => (m, arg) //doesn't change
      case PVar if (isValue(arg)) => { //create new memory
        val a = A.fresh()
        val m2 = m + (a -> arg)
        val deref = Unary(Deref, a) //don't substitute in this function
        (m2, deref)
      }
      case PRef if (isLValue(arg)) => (m, arg) //doesn't change)
      case _ => throw new IllegalArgumentException("Gremlins: Should have checked argApplyable before calling argByMode")
    }
  
    /*** Body ***/
    
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => v1 match{
        case A(a) => println(pretty(m.get(A(a)).get)); (m, Undefined)
        case _ => println(pretty(v1)); (m, Undefined)
      }
      case Unary(Neg, N(n1)) => (m, N(- n1))
      case Unary(Not, B(b1)) => (m, B(! b1))
      case Binary(Seq, v1, e2) if isValue(v1) => (m, e2)
      case Binary(Plus, S(s1), S(s2)) => (m, S(s1 + s2))
      case Binary(Plus, N(n1), N(n2)) => (m, N(n1 + n2))
      case Binary(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => (m, B(inequalityVal(bop, v1, v2)))
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => (m, B(v1 == v2))
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => (m, B(v1 != v2))
      case Binary(And, B(b1), e2) => (m, if (b1) e2 else B(false))
      case Binary(Or, B(b1), e2) => (m, if (b1) B(true) else e2)
      case Binary(Minus, N(n1), N(n2)) => (m, N(n1 - n2))
      case Binary(Times, N(n1), N(n2)) => (m, N(n1 * n2))
      case Binary(Div, N(n1), N(n2)) => (m, N(n1 / n2))
      case If(B(b1), e2, e3) => (m, if (b1) e2 else e3)
      case Obj(fields) if !(fields.values forall isValue) => { //don't think this does anything
         def f(mem: Mem, ex: Expr) : Option[(Mem, Expr)] = if (!isValue(ex)) Some(step(mem, ex)) else None
         val (q1, q2) = mapFirstWith(m, (f _).tupled)(fields.values toList)
        val fields2 = (fields.keys toList) zip q2 toMap;
        (q1, Obj( fields2))
      }
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) =>
        val a = A.fresh()
        (m + (a -> e), a)     
      case Call(v1, args) if isValue(v1) =>
        v1 match {
          case Function(p, params, _, e1) =>
            val zippedpa = (params zip args)
            if (zippedpa.forall { case ((_, (modei, _)), argi) => argApplyable(modei, argi) }) {
              /* DoCall cases */
              val (mp,e1p) = zippedpa.foldRight((m, e1)){
                case (((xi, (modei, _)), argi), (accm, acce)) => 
                  val (accmp, argip) = argByMode(modei, accm, argi)
                  (accmp, substitute(acce, argip, xi))
              }
              p match {
                case None => (mp, e1p)
                case Some(x1) => (mp, substitute(e1p, v1, x1))
              }
            }
            else {
              /* SearchCall2 case */
              val (mp, zippedpap) =
                mapFirstWith[((String,(PMode, Typ)),Expr), Mem](m, { case (accm, (pi @ (s, (modei, t)), argi)) => 
                  modei match { 
                    case PConst => {
                      if (!isValue(argi)) { val (mp, argip) = step(m, argi); Some(mp, ((s, (modei, t)), argip)) }
                      else None
                    }
                    case PName => {
                      if (!isValue(argi))  { val (mp, argip) = step(m, argi); Some(mp, ((s, (modei, t)), argip)) } 
                      else Some(m, ((s, (PConst, t)), argi))
                    }
                    case PRef => {
                      if (!isLValue(argi))  { val (mp, argip) = step(m, argi); Some(mp, ((s, (modei, t)), argip)) } 
                      else None
                    }
                    case PVar => {
                      if (!isValue(argi))  { val (mp, argip) = step(m, argi); Some(mp, ((s, (modei, t)), argip)) } 
                      else None
                    }
                  }

                })(zippedpa)
              (mp, Call(v1, zippedpap.unzip._2))
            }
            
          case _ => throw new StuckError(e)
        }
        
        
      /*** Fill-in more cases here. ***/
        //DoConst
      case Decl(Const, x, v1, e2) if isValue(v1)=> {
        v1 match{
          case _ => 
            val ep = substitute(e2, v1, x)
            (m, ep)
        }
        
      }
      //DoVar
      case Decl(Var, x, v1, e2) if isValue(v1) => {
        v1 match{
          case _ => {
            val a = A.fresh()
        val m2 = m + (a -> v1)
        val defref = Unary(Deref, a)
        val ep2 = substitute(e2, defref, x)
        (m2, ep2)
          }
        } 
      }
      //DoGetField
      case GetField(e1, f) if isValue(e1) => 
         e1 match{
           case Null => throw new NullDereferenceError(e1)
           case A(addr) => {
              m.get(A(addr)) match {
                case Some(Obj(fields)) => fields.get(f) match{
                  case Some(x) => (m, x)
                  case None => throw new StuckError(e)
                }
                case _ => throw new StuckError(e)
              }
           }
           case _ => throw new StuckError(e)   
      } 
      
      //DoAssign  
      case Assign(v1, v2) if (isLValue(v1) && isValue(v2)) =>
        v1 match{
	        case Unary(Deref, A(addr)) => {
	        	val m2 = m + (A(addr) -> v2)
	        	(m2, v2)
	        }
	        case GetField(A(addr), s) => {
	          val a = m.get(A(addr)).get	
	          a match{
	            case Obj(fields) => 
	              val ex = fields.get(s).get
	              val m2 = m + (A(addr) -> Obj(fields + (s -> v2)))
	              (m2, v2)
	            case Null => throw new NullDereferenceError(e)
	            case _ => throw new StuckError(e)
	          }
	        }
	        case _ => throw new StuckError(e) 
      }
        
        
      //DoDeref
      case Unary(Deref, e1) => e1 match{
        case A(addr) => m.get(A(addr)) match{
          case Some(a) =>  (m, a)
          case None => throw new StuckError(e) 
        }
        case _ => throw new StuckError(e)
      }
      
      //DoCast
      case Unary(Cast(t), v1) if isValue(v1) => v1 match{
        case Null => (m, Null)
        case Obj(fields) => throw new DynamicTypeError(e)
        case _ => (m, v1)
      }

        
      /* Inductive Cases: Search Rules */
      case Print(e1) => 
          val (mp,e1p) = step(m,e1)
          (mp, Print(e1p))  
        
      case Unary(uop, e1) => uop match{ 
        case Cast(t) => e1 match{
          case Null => (m, Null)
          case _ => val (mp,e1p) = step(m,e1)
          	(mp, Unary(uop, e1p))
        }
        case _ =>
          val (mp,e1p) = step(m,e1)
        (mp, Unary(uop, e1p))
      }
        
      case Binary(bop, v1, e2) if isValue(v1) =>
        val (mp,e2p) = step(m,e2)
        (mp, Binary(bop, v1, e2p))
      case Binary(bop, e1, e2) =>
        val (mp,e1p) = step(m,e1)
        (mp, Binary(bop, e1p, e2))
      case If(e1, e2, e3) =>
        val (mp,e1p) = step(m,e1)
        (mp, If(e1p, e2, e3))
      case Obj(fields) => fields find { case (_, ei) => !isValue(ei) } match {
        case Some((fi,ei)) =>
          val (mp,eip) = step(m,ei)
          (mp, Obj(fields + (fi -> eip)))
        case None => throw new StuckError(e)
      }
        
      /*** Fill-in more cases here. ***/
      case Decl(mut, x, e1, e2) => 
        val (mp, ep) = step(m, e1)
        (mp, Decl(mut, x, ep, e2))
        
      case GetField(e1, f) => e1 match{ 
        case Obj(fields) => {
          val a = A.fresh()
          val m2 = m + (a -> e1)
          (m2, GetField(a, f))
        }
        case _ => val (mp, ep) = step(m, e1); (mp, GetField(ep, f))
      }
        
       case Assign(e1, e2)  if (!isValue(e2) && isLValue(e1)) =>
        val (mp, ep) = step(m, e2)
        (mp, Assign(e1, ep))
        
       case Assign(e1, e2) if !isValue(e1) =>
        val (mp, ep) = step(m, e1)
        (mp, Assign(ep, e2))
        
      case Call(e1, args)  =>
        val (mp, ep) = step(m, e1)
        (mp, Call(ep, args))
        

      /* Everything else is a stuck error. */
      case _ => throw new StuckError(e)
    }
  }

  def iterateStep(m: Mem, e: Expr): (Mem, Expr) =
    if (isValue(e)) (m, e)
    else {
      val (mp, ep) = step(m, e)
      iterateStep(mp, ep)
    }
    
}