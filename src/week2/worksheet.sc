package week2

object worksheet {
  val sum = (a: Int, b: Int) => a + b             //> sum  : (Int, Int) => Int = <function2>

  val mult = (a: Int, b: Int) => a * b            //> mult  : (Int, Int) => Int = <function2>

  def reduce(f: Int => Int, reducer: (Int, Int) => Int, initialValue: Int)(a: Int, b: Int): Int = {
    def loop(acc: Int, x: Int): Int = {
      if (x > b) acc else loop(reducer(acc, f(x)), x + 1)
    }

    loop(initialValue, a)
  }                                               //> reduce: (f: Int => Int, reducer: (Int, Int) => Int, initialValue: Int)(a: In
                                                  //| t, b: Int)Int

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    reduce(f, mult, 1)(a, b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int

  def id(x: Int) = x                              //> id: (x: Int)Int

  def factorial(x: Int): Int = {
    reduce(id, mult, 1)(1, x)
  }                                               //> factorial: (x: Int)Int

  product(x => x + 1)(1, 4)                       //> res0: Int = 120

  factorial(5)                                    //> res1: Int = 120
  
  product(id)_                                    //> res2: (Int, Int) => Int = <function2>

}