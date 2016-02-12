package week3

object rationals {
  val x = new Rational(1, 3)                      //> x  : week3.Rational = 1/3
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 3
  
  val y = new Rational(5,7)                       //> y  : week3.Rational = 5/7
  x.add(y)                                        //> res2: week3.Rational = 22/21
  
  val z = new Rational(3,2)                       //> z  : week3.Rational = 3/2
  
  x.sub(y).sub(z)                                 //> res3: week3.Rational = -79/42
}
class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(other: Rational) = {
    new Rational(numer * other.denom + denom * other.numer, denom * other.denom)
  }
  
  def neg = {
  	new Rational(-numer, denom)
  }
  
  def sub(other: Rational) = {
    add(other.neg)
  }
  
  override def toString() = {
  	numer + "/" + denom
  }
}