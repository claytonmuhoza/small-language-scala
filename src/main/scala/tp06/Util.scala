package tp06

object Util {
  /**
   * Construit une séquence d'applications de la forme t1 t2 ... tk
   *  à partir de terms = List(t1, t2, ..., tk).
   * Rappel : l'application est associative à gauche
   *  (i.e. t1 t2 t3 ~ (t1 t2) t3)
   */
  def buildApp(terms : List[Term]) : Term = ???

  /**  Remplace, dans t2, toutes les occurrences de x par t1. */
  def subst(x : Var, t1 : Term, t2 : Term) : Term = ???
  
  type Context = List[Val]
  
  /** 
   * Remplace, dans t, chaque alias du contexte ctx par le terme qui lui est
   *  associé.
   */
  def inject(ctx : Context, t : Term) : Term = ???
  
  /** Si optT définit un nouvel alias, l'ajouter en tête du contexte ctx. */
  def buildNewCtx(ctx : Context, optT : Option[Term]) : Context = ???
  
  /** t est-il une valeur ? */
    def isVal(t : Term) : Boolean = ???
  
  /** t est-il un terme clos ? */
  def isClosed(t : Term) : Boolean = ???
}