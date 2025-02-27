package tp01.pascal

import scala.annotation.tailrec

/**
 * Une petite application permettant d'afficher les premières lignes du triangle
 * de Pascal.
 * Deux méthodes sont proposées :
 * - l'une simple, mais gourmande en calculs, affiche les valeurs une par une au
 * fil du calcul ;
 * - l'autre plus efficace en temps, mais plus technique et gourmande en mémoire,
 * construit le triangle entièrement avant de l'afficher.
 */
object Pascal:
  def main(args: Array[String]): Unit =
    println("Le triangle de Pascal valeur par valeur :")
    if args.isEmpty then
      println("Usage : Pascal <n>")
      System.exit(1)
    printTriangle1(args(0).toInt)
    println("Le triangle de Pascal en un seul coup :")
    if args.isEmpty then
      println("Usage : Pascal <n>")
      System.exit(1)
    printTriangle2(args(0).toInt)

  private def fact(v: Int): Int =
    if v <= 1 then 1 else fact(v - 1) * v

  /**
   * Renvoie la valeur de la case ("c", "r") du triangle de Pascal
   */
  private def value(n: Int, k: Int): Int = fact(n) / (fact(k) * fact(n - k))

  /**
   * Affiche les "n" premières lignes du triangle de Pascal valeur par valeur
   * (à l'aide de la méthode "value")
   */
  private def printTriangle1(n: Int): Unit =
    (0 to n-1).foreach(row =>
    println((0 to row).map(value(row, _)).mkString(" - "))
    )

  private type Line = List[Int]

  /**
   * Renvoie la ligne suivant "line" dans le triangle de Pascal
   */
  private def nextLine(line: Line): Line = 
    (0 +: line :+ 0).sliding(2).map(_.sum).toList

  /**
   * Renvoie les "n" premières lignes du triangle de Pascal
   */
  private def triangle(n: Int): List[Line] = 
    (0 until n-1).foldLeft(List(List(1)))((acc, _) => acc :+ nextLine(acc.last))
  /**
   * Affiche les "n" premières lignes du triangle de Pascal ligne par ligne
   * (à l'aide de la méthode "triangle")
   */
  private def printTriangle2(n: Int): Unit =
    triangle(n).foreach(line => println(line.mkString(" - ")))


end Pascal