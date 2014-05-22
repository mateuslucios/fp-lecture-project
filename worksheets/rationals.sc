object rationals {

  val x = new Rational(25, 49)                    //> x  : Rational = 25/49
  x.numerator                                     //> res0: Int = 25
  x.denominator                                   //> res1: Int = 49

  val y = new Rational(2, 3)                      //> y  : Rational = 2/3
  x.add(y)                                        //> res2: Rational = 173/147
  x.sub(y)                                        //> res3: Rational = 23/-147

  val num = new Rational(1, 3)
    .sub(new Rational(5, 7))
    .sub(new Rational(3, 2))                      //> num  : Rational = -79/42

  x.less(y)                                       //> res4: Boolean = true
  x.max(y)                                        //> res5: Rational = 2/3

}

class Rational(n: Int, d: Int) {

  private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  val numerator = n
  val denominator = d

  def less(that: Rational): Boolean =
    this.numerator * that.denominator < that.numerator * this.denominator

  def max(that: Rational): Rational =
    if (this.less(that)) that else this

  def add(that: Rational): Rational = {
    new Rational(
      numerator * that.denominator + that.numerator * denominator,
      denominator * that.denominator)
  }

  def sub(that: Rational): Rational = add(that.neg)

  def neg: Rational = new Rational(-numerator, denominator)

  override def toString: String = {
    val g = gcd(numerator, denominator)
    numerator / g + "/" + denominator / g
  }
}