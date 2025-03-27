package tp07

import scala.collection.immutable.ListMap
import TypeChecker.*

class TypeChecker {
  /** Modélise un contexte de typage */
  private type Context = ListMap[Var, Typ]
  
  /** Retourne le type de "t" si "t" est typable, un message d'erreur sinon. */
  def checkType(t: Term): OptTyp = checkType(t, ListMap())

  private def checkType(t: Term, gamma: Context): OptTyp = {
    t match
      case True | False => SomeTyp(Bool) /* [T-True]  et [T-False] */
      case Zero => SomeTyp(Nat) /* T-Zero */
      case Succ(t) =>if checkType(t) == SomeTyp(Nat)  then SomeTyp(Nat) else NoTyp("pas typable") /*[T-Succ]*/
      case Pred(t) =>if checkType(t) == SomeTyp(Nat)  then SomeTyp(Nat) else NoTyp("pas typable") /*[T-Pred]*/
      case IsZero(t) => if checkType(t) == SomeTyp(Nat) then SomeTyp(Bool) else NoTyp("pas typable") /*[T-Iszero]*/
      case Val(x,t) => checkType(t) /*[T-Var]*/
      case Var(n) =>  if  gamma.getOrElse(Var(n),Nat) != Nat then SomeTyp(gamma.getOrElse(Var(n),Nat)) else NoTyp("pas typable")
      case Cond(t1,t2,t3) =>{ /* T-If */
        checkType(t1) match
          case SomeTyp(Bool) => {
            (checkType(t2,gamma), checkType(t3,gamma)) match
              case (type2, typ3) if type2 == typ3 => type2
              case _ => NoTyp("pas typable")
          }
          case _ => NoTyp("pas typable")
      }
      case App(t1,t2) => checkType(t1,gamma) /*T-App*/
      case Abs(t1,typ,t2) => if(gamma.nonEmpty){ /*T-Abs*/
         checkType(t2,gamma + (t1 -> typ))
      }else{
        checkType(t2, gamma + (t1 -> typ)) match 
          case SomeTyp(type2) => SomeTyp(Fct(typ,type2))
          case _ => NoTyp("pas typable")
      }


  }
}

object TypeChecker {
  /**
   * Modélise un type optionnel, avec un message d'erreur quand le typage
   *  n'a pu être réalisé.
   */
  sealed abstract class OptTyp
  case class SomeTyp(typ: Typ) extends OptTyp
  case class NoTyp(msg: String) extends OptTyp  
}