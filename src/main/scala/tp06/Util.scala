package tp06

import tp05.False

object Util {
  /**
   * Construit une séquence d'applications de la forme t1 t2 ... tk
   *  à partir de terms = List(t1, t2, ..., tk).
   * Rappel : l'application est associative à gauche
   *  (i.e. t1 t2 t3 ~ (t1 t2) t3)
   */
  def buildApp(terms : List[Term]) : Term = {
    if (terms.nonEmpty) {
      terms.tail.foldLeft(terms.head)((acc, elem) => App(acc, elem))
    } else {
      throw new Exception()
    }
  }

  /**  Remplace, dans t2, toutes les occurrences de x par t1. */
  def subst(x : Var, t1 : Term, t2 : Term) : Term = {
    t2 match
      case Var(y) => if(x.name==y) t1 else t2

      case Abs(y,t) => if (x.name != y.name) Abs(y,subst(x,t1,t)) else Abs(y,t)

      case App(t3,t4) => App(subst(x,t1,t3),subst(x,t1,t4))

      case Val(Var(x1),Var(x2)) if (x1==x2 && x1==x.name) => Val(x,t1)

      case Val(y, t) => if (x.name != y.name ) Val(y, subst(x, t1, t)) else Val(y, t)

  }
  
  type Context = List[Val]
  
  /** 
   * Remplace, dans t, chaque alias du contexte ctx par le terme qui lui est
   *  associé.
   */
  def inject(ctx: Context, t: Term): Term = {
    ctx match {
      case Nil => t // Si le contexte est vide, on retourne le terme inchangé

      case head :: tail =>
        t match {
          // Si t est une variable et qu'on trouve une correspondance dans le contexte, on la remplace
          case Var(name) =>
            inject(tail, subst(head.x, head.t, t))

          // Si t est une application, on applique inject aux deux sous-termes
          case App(t1, t2) =>
            App(inject(ctx, t1), inject(ctx, t2))

          // Si t est une abstraction, on vérifie si on doit substituer à l'intérieur
          case Abs(x, body) =>
            if (ctx.exists(binding => binding.x.name == x.name)) Abs(x, body) // On ne remplace pas si la variable est déjà liée
            else Abs(x, inject(ctx, body))

          // Si c'est une valeur, on applique inject sur le terme contenu
          case Val(x, ter) =>
            Val(x, inject(ctx, ter))

          // Par défaut, on retourne le terme tel quel
          case _ => t
        }
    }
  }


  /** Si optT définit un nouvel alias, l'ajouter en tête du contexte ctx. */
  def buildNewCtx(ctx : Context, optT : Option[Term]) : Context = {
    optT match
      case Some(Val(x,t)) => Val(x,t)::ctx
      case _ => ctx
  }
  
  /** t est-il une valeur ? */
    def isVal(t : Term) : Boolean = {
      t match
        case Abs(_,_) => true
        case Val(x,t1) => isVal(t1)
        case _ => false
    }

  /** t est-il un terme clos ? */
  def isClosed(t: Term): Boolean = {

    def isLinked(vl: Var, tl: Term): Boolean = {
      tl match {
        case Var(name) => vl.name == name
        case Abs(x, body) => if (vl.name == x.name) true
          else isLinked(vl, body)
        case App(t1, t2) => isLinked(vl, t1) || isLinked(vl, t2)
        case _ => false
      }
    }

    t match {
      case Abs(x, body) => isLinked(x,body)
      case Var(name) => false
      case App(t1, t2) => isClosed(t1) && isClosed(t2)
      case Val(x,ter) => if isVal(ter) then isLinked(x,ter) else false
      case ter if !isVal(ter) => false
      case _ => false
    }
  }


}