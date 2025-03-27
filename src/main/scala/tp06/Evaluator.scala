package tp06

import Util._

class Evaluator {
  /** Réalise un pas d'évaluation, i.e. produit t' tel que t --> t'. */
  private def eval(t : Term) : Term = {
    t match
      case App(Abs(x,tr),t2) if isVal(t2) => subst(x,t2,tr) /*E-AppAbs*/
      case App(t1,t2) if isVal(t1) => App(t1,eval(t2)) /*E-App2*/
      case App(t1,t2) => App(eval(t1),t2) /*E-App1*/
      case Val(v,App(t1, t2)) => Val(v, eval(App(t1, t2)))
      
      
  }
  
  /** Evalue t jusqu'à obtenir un terme bloqué. */
  def evaluate(t : Term) : Term = {
    if (isVal(t)) t else evaluate(eval(t))
  }
}
