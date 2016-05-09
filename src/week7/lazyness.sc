package week7

object lazyness {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def from(n: Int): Stream[Int] = Stream.cons(n,  from(n + 1))
                                                  //> from: (n: Int)Stream[Int]

  from(5).take(5).toList                          //> res0: List[Int] = List(5, 6, 7, 8, 9)
  // val firstten = from5 take 10
  // firstten.toList
  //from(5).take(5).toList()
  
  def improve(x: Double)(guess: Double) = {println("i");(guess + x / guess) / 2}
                                                  //> improve: (x: Double)(guess: Double)Double
          
  lazy val guesses: Stream[Double] = 1 #:: (guesses map improve(10)_)
                                                  //> guesses: => Stream[Double]
  
  guesses                                         //> res1: Stream[Double] = Stream(1.0, ?)
  
  guesses.tail.tail.tail                          //> i
                                                  //| i
                                                  //| i
                                                  //| res2: scala.collection.immutable.Stream[Double] = Stream(3.196005081874647, 
                                                  //| ?)
                                                  
  guesses                                         //> res3: Stream[Double] = Stream(1.0, 5.5, 3.659090909090909, 3.196005081874647
                                                  //| , ?)

  def sqrtStream(x: Double): Stream[Double] = {
    val improveX = improve(x)_
    def guesses(guess: Double): Stream[Double] = guess #:: guesses(improveX(guess))
    guesses(1)
  }                                               //> sqrtStream: (x: Double)Stream[Double]
  
  def sqrtStreamOriginal(x: Double): Stream[Double] = {
    val improveX = improve(x)_
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improveX)
    guesses
  }                                               //> sqrtStreamOriginal: (x: Double)Stream[Double]
  
  def sqrtStreamOriginalWithCons(x: Double): Stream[Double] = {
    val improveX = improve(x)_
    lazy val guesses: Stream[Double] = Stream.cons(1, guesses map improveX)
    guesses
  }                                               //> sqrtStreamOriginalWithCons: (x: Double)Stream[Double]
  
  sqrtStream(10.0).take(3).toList                 //> i
                                                  //| i
                                                  //| res4: List[Double] = List(1.0, 5.5, 3.659090909090909)
  sqrtStreamOriginal(10.0).take(3).toList         //> i
                                                  //| i
                                                  //| res5: List[Double] = List(1.0, 5.5, 3.659090909090909)
  sqrtStreamOriginalWithCons(10.0).take(3).toList //> i
                                                  //| i
                                                  //| res6: List[Double] = List(1.0, 5.5, 3.659090909090909)
   
  val multiplesOfTwo: Stream[Int] = from(1) map (_ * 2)
                                                  //> multiplesOfTwo  : Stream[Int] = Stream(2, ?)
  
  lazy val powersOfTwo: Stream[Int] = 1 #:: (powersOfTwo map (_ * 2))
                                                  //> powersOfTwo: => Stream[Int]
  
  powersOfTwo.take(10).toList                     //> res7: List[Int] = List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512)
}