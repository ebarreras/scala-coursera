package week2

import scala.annotation._

object worksheet {
  val sum = (a: Int, b: Int) => a + b

  val mult = (a: Int, b: Int) => a * b

  def reduce(f: Int => Int, combine: (Int, Int) => Int, initialValue: Int)(a: Int, b: Int): Int = {
    def loop(acc: Int, x: Int): Int = {
      if (x > b) acc else loop(combine(acc, f(x)), x + 1)
    }

    loop(initialValue, a)
  }

  def reduce2(f: Int => Int, combine: (Int, Int) => Int, initialValue: Int)(a: Int, b: Int): Int = {
      if (a > b) initialValue else reduce2(f, Int.+(_), combine(initialValue,f(a)))(a + 1, b)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    reduce(f, mult, 1)(a, b)
  }

  def id(x: Int) = x

  def factorial(x: Int): Int = {
    reduce(id, mult, 1)(1, x)
  }
  
  def factorial2(x: Int): Int = {
    reduce2(id, mult, 1)(1, x)
  }

  product(x => x + 1)(1, 4)

  factorial(5)
  
  factorial2(5)
  
  product(id)_
  
  Int::+

}