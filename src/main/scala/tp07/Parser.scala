package tp07

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * prog --> '\z'
 *   |  cmd ";;"
 * cmd --> t
 *   |  val v '=' t
 * t --> open
 *   |  atom+ t?
 * open --> lambda x':' typ'.' t
 *   |  if t then t else t
 *   |  succ t
 *   |  pred t
 *   |  iszero t
 * atom --> x
 *   |  '('t')'
 *   |  true
 *   |  false
 *   |  0
 * typ --> atomTyp ("->" atomTyp)*
 * atomTyp --> '('typ')'
 *   | Bool
 *   | Nat
 * Indication : le non-terminal "typ" engendre un atomTyp suivi d'une séquence
 *  éventuellement vide de ('->' atomTyp). On gère cette situation de manière
 *  similaire à la séquence d'applications traitée la séance précédente. Cette
 *  fois, on fera appel à la méthode "Util.buildFctType" pour construire le
 *  type fonctionnel en cascade à partir d'une liste de types. 
 */
class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|#.*)+""".r
  override val ident = """[a-zA-Z][a-zA-Z0-9]*""".r
  def keywords = Set("val")
  
  def prog : Parser[Term] = eof | cmd<~";;"
  def cmd : Parser[Term] = term
      | ("val"~>variable)~("="~>term) ^^ { case x~t => Val(x, t) }
  def eof = """\z""".r ^^ { _ => EOF}
  def term : Parser[Term] = ???
  def open = lambda | cond | succ | pred| isZero
  def lambda = ("lambda" ~> variable <~ ":") ~ typ ~ ("." ~> term) ^^ {
    case x~typ~t => Abs(x, typ, t)
  }
  def cond = ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ {
    case t1~t2~t3 => Cond(t1, t2, t3)
  }
  def succ = ("succ" ~> term) ^^ { case t => Succ(t) }
  def pred = "pred"~>term ^^ {
    case t => Pred(t)
  }
  def isZero = "iszero"~> term ^^ {
    case t => IsZero(t)
  }
  def atom = variable | "(" ~> term <~ ")" | tTrue | tFalse | zero
  def variable =  ident.filter(!keywords.contains(_)) ^^{name => Var(name)}
  def tFalse = "false" ^^ { _ => False }
  def tTrue = "true" ^^ { _ => True }
  def zero = "0" ^^ { _ => Zero }
  def typ : Parser[Typ] = atomTyp ~ rep("->" ~> atomTyp) ^^ {
    case typ~typs => Util.buildFctType(typ::typs)
  }
  def atomTyp = "(" ~> typ <~ ")" | bool | nat
  def bool = "Bool" ^^ { _ => Bool }
  def nat = "Nat" ^^ { _ => Nat }
}
