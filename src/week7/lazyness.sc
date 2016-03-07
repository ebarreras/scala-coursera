package week7

object lazyness {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def from(n: Int): Stream[Int] = Stream.cons(n,  from(n + 1))
                                                  //> from: (n#2737820: Int#1121)Stream#2300493[Int#1121]

  from(5).take(5).toList                          //> res0: List#2300479[Int#1121] = List(5, 6, 7, 8, 9)
  // val firstten = from5 take 10
  // firstten.toList
  //from(5).take(5).toList()

  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = {println("i");(guess + x / guess) / 2}
    def guesses(guess: Double): Stream[Double] = guess #:: guesses(improve(guess))
    guesses(1)
  }                                               //> sqrtStream: (x#2738238: Double#1637)Stream#2300493[Double#1637]
  
  def sqrtStreamOriginalWithCons(x: Double): Stream[Double] = {
    def improve(guess: Double) = {System.out.println("i");(guess + x / guess) / 2}
    lazy val guesses: Stream[Double] = Stream.cons(1, guesses map improve)
    guesses
  }                                               //> sqrtStreamOriginalWithCons: (x#2738758: Double#1637)Stream#2300493[Double#16
                                                  //| 37]
  
  def sqrtStreamOriginal(x: Double): Stream[Double] = {
    def improve(guess: Double) = {println("i");(guess + x / guess) / 2}
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }                                               //> sqrtStreamOriginal: (x#2738838: Double#1637)Stream#2300493[Double#1637]
  
  sqrtStream(10.0).take(3).toList                 //> i
                                                  //| i
                                                  //| res1: List#2300479[Double#1637] = List(1.0, 5.5, 3.659090909090909)
  sqrtStreamOriginal(10.0).take(3).toList         //> i
                                                  //| i
                                                  //| res2: List#2300479[Double#1637] = List(1.0, 5.5, 3.659090909090909)
   
  val multiplesOfTwo: Stream[Int] = from(1) map (_ * 2)
                                                  //> multiplesOfTwo  : Stream#2300493[Int#1121] = Stream(2, ?)
  
  lazy val powersOfTwo: Stream[Int] = 1 #:: (powersOfTwo map (_ * 2))
                                                  //> powersOfTwo: => Stream#2300493[Int#1121]
  
  powersOfTwo.take(10).toList                     //> res3: List#2300479[Int#1121] = List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512)
}