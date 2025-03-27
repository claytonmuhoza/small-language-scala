package tp06

import java.io.FileReader
import java.io.PrintStream
import java.io.File
import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.Reader
import scala.Console.*
import Util.*

import java.io.FileOutputStream
import scala.annotation.tailrec

class Interpretor {
  val parser = new Parser
  val evaluator = new Evaluator
  val formatter = new Formatter

  def interpret(reader : Reader[Char]) : Unit = {
    Console.withOut(new PrintStream(new File("src/test/scala/tp06/results.txt"))) {
      interpret(reader, List())
    }
  }

  /**
   * Interprète le flux fourni par reader dans le contexte ctx.
   * Une analyse syntaxique du flux est réalisée par le parser pour construire
   *  les termes associés aux commandes du flux.
   * Au fur et à mesure, le contexte (une liste de variables globales et leur
   *  valeur) est injecté dans les termes et, pour chaque terme clos obtenu
   *  après injection, les opérations suivantes sont réalisées :
   * - évaluation du terme
   * - formatage du terme obtenu par l'évaluation
   * - affichage du terme formaté
   * Le contexte est enrichi au fil de l'interprétation, chaque fois qu'un
   *  terme de la forme "val x = t" est rencontré.
   * Un message signale l'apparition d'un terme non-clos dans le flux. Un tel
   *  terme n'est pas évalué.
   * Un message d'erreur est produit en cas d'erreur syntaxique repérée par le
   *  parser.
   * Indication : pour alléger cette méthode, on fera appel à la méthode
   *  "interpret(t: Term)".
   */
  @tailrec
  private def interpret(reader: Reader[Char], ctx: Context): Unit = {
    val terms = parser.prog(reader)

    terms match {
      case parser.Success(t, resRead) =>
        t match {
          case Val(x, ter) =>
            val newCtx = buildNewCtx(ctx, Some(Val(x, ter)))
            val injectedTerm = inject(newCtx, t)
            val resInter = interpret(injectedTerm)
            interpret(resRead, newCtx)
          case EOF => println("Fin du programme")
          case _ =>
            println(s"$t")
            val injectedTerm = inject(ctx, t)

            val resInter = interpret(injectedTerm)
            interpret(resRead, ctx)
        }

      case parser.Failure(msg, _) =>
        println(s"Parsing failed: $msg")
    }
  }


  /**
   * Teste si t est un terme clos.
   * Si la réponse est négative, on formate le terme que l'on affiche sur la
   *  sortie standard et on signale qu'il est non clos.
   * Sinon, on évalue t, on affiche le terme bloqué obtenu et on le retourne.
   */
  private def interpret(t: Term): Option[Term] = {
    if (!isClosed(t)) {
      println(s"${formatter.format(t)} Terme non clos\n")
      None
    } else {
      try {
        val terEval = evaluator.evaluate(t)
        println(s"${formatter.format(terEval)}\n")
        Some(terEval)
      }catch
        case e: Exception =>
          println(s"Échec de l'évaluation du terme : ${formatter.format(t)}\n")
          println(s"Détails de l'erreur : ${e.getMessage}")
          None

    }
  }
}

object Main {
  def main(args : Array[String]) = {
    val reader = StreamReader(new FileReader("src/test/scala/tp06/examples.txt"))
    new Interpretor().interpret(reader)
  }
}