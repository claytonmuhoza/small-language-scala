package tp05

sealed trait Term
case object EOF extends Term
case object True extends Term
case object False extends Term
case class Cond(t1:Term, t2:Term, t3:Term) extends Term
case class IsZero(t:Term) extends Term
case class Succ(t:Term) extends Term
case class Pred(t:Term) extends Term
case object Zero extends Term