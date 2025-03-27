package tp07

sealed trait Term
case object EOF extends Term
case class Val(x : Var, t : Term) extends Term
case class Var(name : String) extends Term
case class App(t1: Term, t2: Term) extends Term
case class Abs(x: Var, typ: Typ,t: Term) extends Term
case class Succ(t: Term) extends Term
case class Pred(t: Term) extends Term
case class IsZero(t: Term) extends Term
case class Cond(t1: Term, t2: Term, t3: Term) extends Term
case object True extends Term

case object False extends Term

case object Zero extends Term

sealed trait Typ
object Bool extends Typ
object Nat extends Typ
case class Fct(typ1: Typ, typ2: Typ) extends Typ

