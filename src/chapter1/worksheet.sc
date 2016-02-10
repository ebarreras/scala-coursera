package chapter1

import math._
import scala.annotation.tailrec

object worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def sqrtIter(a: Double, x: Double): Double = {
    if (isGoodEnough(a, x))
      a
    else
      sqrtIter(improve(a, x), x)
  }                                               //> sqrtIter: (a: Double, x: Double)Double

  def isGoodEnough(a: Double, x: Double): Boolean = {
    abs(a * a - x) < x * 0.0001
  }                                               //> isGoodEnough: (a: Double, x: Double)Boolean

  def improve(a: Double, x: Double): Double = {
    (a + x / a) / 2
  }                                               //> improve: (a: Double, x: Double)Double

  def sqrt(x: Double): Double = {
    sqrtIter(1, x)
  }                                               //> sqrt: (x: Double)Double

  sqrt(2)                                         //> res0: Double = 1.4142156862745097

  sqrt(10e-6)                                     //> res1: Double = 0.0031622926477232706

  sqrt(10e48)                                     //> res2: Double = 3.162308428500741E24
  
  def tailFactorial(x: Integer): Integer = {
  	
  	@tailrec
  	def iter(i: Integer, a: Integer): Integer = {
  		if (i == 0) a else iter(i-1, i*a)
  	}
  
  	iter(x, 1)
  }                                               //> tailFactorial: (x: Integer)Integer
  
  def factorial(x: Integer): Integer = {
  		if (x == 0) 0 else x*factorial(x-1)
  }                                               //> factorial: (x: Integer)Integer

	tailFactorial(3)                          //> res3: Integer = 6
	
	factorial(3)                              //> res4: Integer = 0
}