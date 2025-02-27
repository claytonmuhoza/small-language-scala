package tp01.complex

/**
 * Une classe modélisant les nombres complexes
 */
class Complex(val real: Double, val imag: Double):
  /**
   * Pour afficher élégamment les nombres complexes, y compris quand la partie
   * réelle est nulle ou quand la partie imaginaire vaut -1, 0 ou 1
   */
  override def toString: String = 
      (real, imag) match
      case (0, 0) => "0"
      case (0, _) if imag == 1 => "i"
      case (0, _) if imag == -1 => "-i"
      case (0, _) => s"${imag}i"
      case (_, 0) => s"$real"
      case (_, _) if imag == 1 => s"$real + i"
      case (_, _) if imag == -1 => s"$real - i"
      case (_, _) if imag < 0 => s"$real - ${-imag}i"
      case (_, _) => s"$real + ${imag}i"
  /**
   * Le module du nombre complexe
   * rappel : module(a + bi) = sqrt(a * a + b * b)
   */
  def mod: Double = Math.sqrt(real * real + imag * imag)
  /**
   * L'argument d'un nombre complexe
   * rappel : argument(c = a + bi) = acos(a / module(c))
   */
  def arg: Double = Math.acos(real / mod)
  /**
   * Le complexe obtenu en additionnant "this" et "that"
   */
  def +(that: Complex): Complex = Complex(real + that.real, imag + that.imag)
  /**
   * Le complexe obtenu en additionnant "this" et "that"
   */

  def +(that: Double): Complex = Complex(real + that, imag)
  /**
   * Le complexe obtenu en additionnant "this" et un entier "that"
   */
  
  def +(that: Int): Complex = Complex(real + that, imag)

  /**
   * Le complexe obtenu en soustrayant "that" à "this"
   */
  def -(that: Complex): Complex = Complex(real - that.real, imag - that.imag)
  /**
   * Le complexe obtenu en soustrayant "that" à "this"
   */
  def -(that: Double) = Complex(real - that, imag)
  /**
   * Le complexe obtenu en multipliant "this" et "that"
   */
  def *(that: Complex) = Complex(real * that.real - imag * that.imag, real * that.imag + imag * that.real)
  /**
   * Le complexe obtenu en multipliant "this" et "that"
   */
  def *(that: Double) = Complex(real * that, imag * that)
  /**
   * Le complexe obtenu en divisant "this" par "that"
   */
  def /(that: Complex) = Complex(real / that.real, imag / that.imag)
  /**
   * Le complexe obtenu en divisant "this" par "that"
   */
  def /(that: Double) = Complex(real / that, imag / that)
  /**
   * Le complexe conjugué de "this"
   * rappel : conj(a + bi) = a - bi
   */
  def conj = Complex(real, -imag)
  /** 
   * Vérifie l'égalité entre deux nombres complexes
   */
  override def equals(obj: Any): Boolean = obj match
    case that: Complex => this.real == that.real && this.imag == that.imag
    case _ => false
  // override def equals(obj: Any): Boolean =
  //   obj match
  //     case that: Complex => this.real == that.real && this.imag == that.imag
  //     case _ => false

  /**
   * HashCode basé sur les parties réelle et imaginaire
   */
  override def hashCode(): Int =
    (real, imag).##

  /**
   * Comparaison de deux nombres complexes
   */
  def >(that: Complex): Boolean =  (this.real == that.real && this.imag > that.imag) || (this.real > that.real)

  /**
    * Comparaison de deux nombres complexes
    */
  def <(that: Complex): Boolean = (this.real == that.real && this.imag < that.imag) || (this.real < that.real)

end Complex
/**
  * Initialisation d'un complexe par coordonnées polaires
  */
class  PolarComplex(mod: Double, arg: Double)
  extends Complex(mod * Math.cos(arg),mod * Math.sin(arg))

extension (i : Int)
    def +(c: Complex): Complex = Complex(i + c.real, c.imag)