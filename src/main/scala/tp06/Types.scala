package tp06

sealed trait Term
case object EOF extends Term
case class Var(name : String) extends Term
case class Val(x : Var, t : Term) extends Term
case class App(t1:Term, t2: Term) extends Term
case class Abs(x : Var, t: Term) extends Term
