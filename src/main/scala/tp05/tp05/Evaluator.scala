package tp05

import Util._

class Evaluator {
  

  /** Réalise un pas d'évaluation, i.e. produit t' tel que t --> t'. */
  private def eval(t : Term) : Term = t match {
    case Cond(True, t2, _) => t2
    case Cond(False, _, t3) => t3
    case Cond(t1, t2, t3) => Cond(eval(t1), t2, t3)
    case IsZero(Zero) => True
    case IsZero(Succ(n)) if isNumVal(n) => False
    case IsZero(t) =>
      val t1 = eval(t)
      IsZero(t1)
    case Succ(n) if isNumVal(n) => Succ(n)
    case Succ(t) =>
      val t1 = eval(t)
      Succ(t1)
    case Pred(Zero) => Zero
    case Pred(Succ(n)) if isNumVal(n) => n
    case Pred
    (t) =>
      val t1 = eval(t)
      Pred(t1)
  }
  /** Evalue t jusqu'à obtenir un terme bloqué. */
  def evaluate(t : Term) : Term = {
    if (isVal(t)) t else evaluate(eval(t))
  }
}
