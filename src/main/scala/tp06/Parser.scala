package tp06

import scala.language.postfixOps
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * prog --> '\z'
 *   | cmd ";;"
 * cmd --> term
 *   |  val v '=' term
 * term --> lambda x'.' term
 *   |  atom+ term?
 * atom --> x
 *   | '('term')'
 *   
 * Indication : la 6ème production génère une séquence non-vide d'"atom"
 *  terminée par une lambda-abstraction optionnelle.
 *  Le combinateur "+" produit une liste de termes que l'on transformera
 *  en une cascade d'applications grâce à la méthode "Util.buildApp".
 *  
 * Indication : les variables respectent le motif [a-zA-Z][a-zA-Z0-9]* et ne
 *  doivent pas être des mots clés du langage.
 */
class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|#.*)+""".r
  override val ident = """[a-zA-Z][a-zA-Z0-9]*""".r
  def keywords = Set("val","lambda")
  
  def prog : Parser[Term] = eof | cmd<~";;"
  def cmd : Parser[Term] = term
      | ("val"~>variable)~("="~>term) ^^ { case x~t => Val(x, t) }
  def eof = """\z""".r ^^ { _ => EOF}
  
  def term : Parser[Term] = lambda | (atom+) ~ opt(lambda) ^^ {
    case atoms ~ Some(abs) => Util.buildApp(atoms :+ abs)
    case atoms ~ None => Util.buildApp(atoms)
  }
  def lambda = ("lambda" ~> variable <~ ".") ~ term ^^ { case v ~ t => Abs(v, t) }

  def atom: Parser[Term] =
    variable ^^ { v => v }  
      | "(" ~> term <~ ")" 
  def variable = ident.filter(!keywords.contains(_)) ^^{name => Var(name)}
}
