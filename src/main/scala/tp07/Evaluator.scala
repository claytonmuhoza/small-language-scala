package tp07

import Util._

class Evaluator {
  /** Réalise un pas d'évaluation, i.e. produit t' tel que t --> t'. */
  private def eval(t : Term) : Term = {
    t match
      case App(Abs(x,typ,tr),t2) if isVal(t2) => subst(x,t2,tr) /*E-AppAbs*/
      case App(t1,t2) if isVal(t1) => App(t1,eval(t2)) /*E-App2*/
      case App(t1,t2) => App(eval(t1),t2) /*E-App1*/
      case Val(v,App(t1, t2)) => Val(v, eval(App(t1, t2)))
      case Cond(t1, t2, t3) => {

        t1 match
          case True => t2 /*E-IfTrue*/
          case False => t3 /*E-IfFalse*/
          case _ => Cond(eval(t1),t2,t3) /*E-If*/

      }
      case IsZero(ter) => {
        ter match
          case Zero => True /*E-IfZeroZero*/
          case Succ(nv) if (isNumVal(nv)) => False /*E-IfZeroSucc*/
          case _ => IsZero(eval(ter)) /*E-IfZero*/
      }
      case Pred(ter) => {
        ter match
          case Zero => Zero /*E-PredZero*/
          case Succ(nv) if isNumVal(nv) => nv /*E-PredSucc*/
          case _ => Pred(eval(ter)) /*E-Pred*/
      }

      case Succ(ter) => {
        ter match
          case _ => Succ(eval(ter)) /*E-Succ*/

      }

      case _ => throw new Exception()
  }
  
  /** Evalue t jusqu'à obtenir un terme bloqué. */
  def evaluate(t : Term) : Term = {
    if (isVal(t)) t else evaluate(eval(t))
  }
}
