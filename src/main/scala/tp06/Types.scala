package tp06

sealed trait Term
case object EOF extends Term
case class Var(name : String) extends Term
case class Val(x : Var, t : Term) extends Term
