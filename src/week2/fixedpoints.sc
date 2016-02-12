package week2

import math.abs

object fixedpoints {
	val tolerance = 0.001                     //> tolerance  : Double = 0.001

	def isGoodEnough(x: Double, y: Double): Boolean =
		abs((x - y)/x)/x < tolerance      //> isGoodEnough: (x: Double, y: Double)Boolean

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def loop(guess: Double): Double = {
      val next = f(guess)
      if (isGoodEnough(guess, next))
        guess
      else
        loop(next)
    }
		loop(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) /2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double
  
	def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1)
                                                  //> sqrt: (x: Double)Double
	fixedPoint(x => 1 + x/2)(1)               //> res0: Double = 1.9921875
	
	sqrt(2)                                   //> res1: Double = 1.4142156862745097
}