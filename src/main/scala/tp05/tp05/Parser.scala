package tp05

import scala.util.parsing.combinator.RegexParsers

/**
 * t --> '('t')'
 *    |  '\z'
 *    |  false
 *    |  true
 *    |  if t then t else t
 *    |  0
 *    |  succ t
 *    |  pred t
 *    |  iszero t
 */
class Parser extends RegexParsers {
def term : Parser[Term] = parenTerm | eof | falseTerm | trueTerm  | zeroTerm  | ifTerm  | succTerm | predTerm | isZero 
  def parenTerm = ("("~>term<~")")
  def eof = """\z""".r ^^ { _ => EOF }
  def falseTerm = "false" ^^ { _ => False }
  def trueTerm = "true" ^^ {_ => True }
  def ifTerm = "if"~>term~"then"~term~"else"~term ^^ {
    case t1~_~t2~_~t3 => Cond(t1, t2, t3)
  }
  def zeroTerm = """0""".r ^^ {_ => Zero}
  def succTerm = "succ"~>term ^^ {
    case t => Succ(t)
  }
  def predTerm = "pred"~>term ^^ {
    case t => Pred(t)
  }
  def isZero = "iszero"~> term ^^ {
    case t => IsZero(t)
  }
  
}
