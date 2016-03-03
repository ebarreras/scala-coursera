package week6

object foreipressions {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def isPrime(n: Int): Boolean = (2 to n / 2).forall(n % _ != 0)
                                                  //> isPrime: (n: Int)Boolean

  def findPrimesHard(n: Int) =
    (1 until n) flatMap (i => (i + 1 until n) map (j => (i, j))) filter { case (i, j) => isPrime(i + j) }
                                                  //> findPrimesHard: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]

  def findPrimes(n: Int) =
    for {
      i <- 1 until n
      j <- i + 1 until n
      if isPrime(i + j)
    } yield (i, j)                                //> findPrimes: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]

  findPrimes(7)                                   //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,2), (1,4
                                                  //| ), (1,6), (2,3), (2,5), (3,4), (5,6))

  def scalarProduct(xs: Seq[Double], ys: Seq[Double]): Double =
    (for { (x, y) <- xs zip ys } yield x * y).sum //> scalarProduct: (xs: Seq[Double], ys: Seq[Double])Double

  val xs = Array(1.3, 7, 4, 3, 6)                 //> xs  : Array[Double] = Array(1.3, 7.0, 4.0, 3.0, 6.0)

  scalarProduct(xs, xs)                           //> res1: Double = 111.69
}