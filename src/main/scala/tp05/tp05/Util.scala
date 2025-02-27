package tp05

object Util {
  
  /** t est-il une valeur ? */
  def isVal(t : Term) : Boolean = t match
    case True => true
    case False => true
    case _  => isNumVal(t)
  

  /** t est-il une valeur numérique ? */
  def isNumVal(t : Term) : Boolean = t match
    case Zero => true
    case Succ(x) => isNumVal(x)
    case _ => false
}