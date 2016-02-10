package chapter1

import math._

object worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  3                                               //> res0: Int(3) = 3
  4                                               //> res1: Int(4) = 4

  def sqrtIter(a: Double, x: Double): Double = {
    if (abs(a * a - x) < x*0.0001)
      a
    else
      sqrtIter((a + x / a) / 2, x)
  }                                               //> sqrtIter: (a: Double, x: Double)Double

  def sqrt(x: Double): Double = {
    sqrtIter(1, x)
  }                                               //> sqrt: (x: Double)Double


	sqrt(10e-6)                               //> res2: Double = 0.0031622926477232706
	
	sqrt(10e48)                               //> res3: Double = 3.162308428500741E24
	
}