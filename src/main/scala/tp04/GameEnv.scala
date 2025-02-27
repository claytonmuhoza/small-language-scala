package tp04

/**
 * Le type "GameEnv" modélise un environnement de jeu (le terrain, les cases
 *  de départ et d'arrivée, les concepts de position, bloc et mouvement).
 */
trait GameEnv {
  /** La classe "Pos" modélise une position sur le terrain de jeu.
   *  Rq : l'indice des lignes augmente quand on se déplace vers le bas.
   *       l'indice des colonnes augmente quand on se déplace vers la droite.
   */
  case class Pos(row: Int, col: Int) {
    /** La position obtenue en se décalant de "d" lignes. */
    def deltaRow(d: Int): Pos = copy(row = row + d)
    
    /** La position obtenue en se décalant de "d" colonnes. */
    def deltaCol(d: Int): Pos = copy(col = col + d)
  }

  /**
   * La position où se trouve le bloc au départ du jeu.
   * Cette valeur est abstraite. Elle sera fournie lors de la définition d'un
   *  jeu concret.
   */
  lazy val start: Pos

  /**
   * La cible à atteindre avec le bloc.
   * Cette valeur aussi est abstraite.
   */
  lazy val goal: Pos

  /**
   * Le type modélisant le terrain de jeu : une fonction booléenne qui vaut
   *  vrai ssi on lui fournit la position d'une case présente sur le terrain.
   */
  type Playground = Pos => Boolean

  /**
   * Le terrain de jeu.
   * Cette valeur aussi est abstraite.
   */
  lazy val playground: Playground

  /** Le type "Move" modélisant les mouvements possibles pour le bloc. */
  sealed abstract class Move
  case object Left  extends Move
  case object Right extends Move
  case object Up    extends Move
  case object Down  extends Move

  /**
   * Le bloc est caractérisé par la position de ses deux moitiés.
   * La première est toujours inférieure ou égale à la seconde dans l'ordre
   *  lexicographique (ligne - colonne).
   */
  case class Block(p1: Pos, p2: Pos) {
    /** Le bloc est-il à la verticale ? */
    def isStanding: Boolean = p1 == p2

    /** Les deux moitiés du bloc sont-elles sur le terrain de jeu ? */
    def isLegal: Boolean = playground(p1) && playground(p2)

    /**
     * Retourne le bloc obtenu en décalant la première moitié de "d1" lignes
     *  et la seconde de "d2" lignes.
     */
    def deltaRow(d1: Int, d2: Int) = {
      Block(p1.deltaRow(d1), p2.deltaRow(d2))
    }

       
      
    

    /**
     * Retourne le bloc obtenu en décalant la première moitié de "d1" colonnes
     *  et la seconde de "d2" colonnes.
     */
    def deltaCol(d1: Int, d2: Int) = {Block(p1.deltaCol(d1),p2.deltaCol(d2))}

    /** Le bloc obtenu en se déplaçant vers la gauche. */
    def left = if(isStanding) deltaCol(-1,-2) else if(p1.col == p1.col) deltaCol(-1,-1) else deltaCol(-1,-2)

    /** Le bloc obtenu en se déplaçant vers la droite. */
    def right = if(isStanding) deltaCol(1,2) else if(p1.col == p2.col && !isStanding) deltaCol(1,1) else deltaCol(1,2)

    /** Le bloc obtenu en se déplaçant vers le haut. */
    def up = if(p1.row == p2.row && !isStanding) deltaRow(-1,-1) else deltaRow(-1, -2)

    /** Le bloc obtenu en se déplaçant vers le bas. */
    def down = if(p1.row == p2.row && !isStanding) deltaRow(1, 1) else deltaRow(1,2)

    /**
     * Retourne la liste des blocs que l'on peut obtenir en un mouvement.
     * Chaque bloc est accompagné du mouvement qui a permis de l'obtenir.
     */
    def neighbours: List[(Block, Move)] = ???

    /**
     * Similaire à la méthode "neighbours", mais ne conserve que les blocs
     *  légaux.
     */
    def validNeighbours: List[(Block, Move)] = ???
  }
}