object rational {
  val a = new Rational(1,3)
  a.numer
  a.denom

  def addRational (r: Rational, s: Rational): Rational =
    new Rational(
      r.numer * s.denom + s.numer* s.denom, r.denom* s.denom)

  def makeString(r: Rational)=
    r.numer + "/" + r.denom

  print(makeString(addRational(new Rational(1,2), new Rational(2, 3))))

  val b = new Rational(2,3)
  a.add(b)

  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  x.add(y)
  x.sub(y).sub(z)
  x - y - z

  y.add(y)
  y + y

  x.less(y)
  x < y

  x.max(y)
  x max y

  //require
  //val strange = new Rational(1,0)
  //strange.add(strange)

  //constructors
  new Rational(2)


}

class Rational(x:Int, y: Int){
  //Preconditions
  require(y != 0, "Denominator must be nonzero")

  //constructors
  def this(x: Int) = this(x, 1)

  //precedence operators:
  //a + b ^? c ?^ d less a ==> b | c
  //((a + b) ^? (c ?^ d)) less ((a ==> b) | c)


  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  //private val g = gcd(x, y)
//  def numer = x / g
//  def denom = y / g

  def numer = x
  def denom = y

  def less(that: Rational) = numer * that.denom < that.numer * denom
  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if(this.less(that)) that else this

  def max1(that: Rational) = if(this < that) that else this

  def add(that: Rational) =
    new Rational(numer * that.denom +that.numer* denom, denom * that.denom)

  def + (that: Rational) =
    new Rational(numer * that.denom +that.numer* denom, denom * that.denom)


  def neg: Rational = new Rational(-numer, denom)
  def unary_- : Rational = new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg)
  //def - (that: Rational) = this + that.neg
  def - (that: Rational) = this + -that

  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }


}
