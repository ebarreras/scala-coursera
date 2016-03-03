package week6

object vectors {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val numbers = 2 +: 4 +: 5 +: 9 +: 6 +: 5 +: Vector()
                                                  //> numbers  : scala.collection.immutable.Vector[Int] = Vector(2, 4, 5, 9, 6, 5)
                                                  //| 
  
  val xs = Array(1.3, 7, 4, 3, 6)                 //> xs  : Array[Double] = Array(1.3, 7.0, 4.0, 3.0, 6.0)
  
  xs.map(_ * 2)                                   //> res0: Array[Double] = Array(2.6, 14.0, 8.0, 6.0, 12.0)
  
  val s = "Hello World!"                          //> s  : String = Hello World!
  
  s.tail                                          //> res1: String = ello World!
  
  s.filter(_.isUpper)                             //> res2: String = HW
  
  1.to(5)                                         //> res3: scala.collection.immutable.Range.Inclusive = Range(1, 2, 3, 4, 5)

	xs.zip(s)                                 //> res4: Array[(Double, Char)] = Array((1.3,H), (7.0,e), (4.0,l), (3.0,l), (6.0
                                                  //| ,o))
	
	s.zip(xs)                                 //> res5: scala.collection.immutable.IndexedSeq[(Char, Double)] = Vector((H,1.3)
                                                  //| , (e,7.0), (l,4.0), (l,3.0), (o,6.0))
	
  xs.sum                                          //> res6: Double = 21.3
  
  val M = 5                                       //> M  : Int = 5
  
  val N = 7                                       //> N  : Int = 7
  
  1 to M flatMap ( m => 1 to N map (n => (m, n))) //> res7: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1,2
                                                  //| ), (1,3), (1,4), (1,5), (1,6), (1,7), (2,1), (2,2), (2,3), (2,4), (2,5), (2,
                                                  //| 6), (2,7), (3,1), (3,2), (3,3), (3,4), (3,5), (3,6), (3,7), (4,1), (4,2), (4
                                                  //| ,3), (4,4), (4,5), (4,6), (4,7), (5,1), (5,2), (5,3), (5,4), (5,5), (5,6), (
                                                  //| 5,7))
  
  def scalarProduct(xs: Seq[Double], ys: Seq[Double]) =
      ((xs zip ys) map {case (x, y) => x * y}).sum//> scalarProduct: (xs: Seq[Double], ys: Seq[Double])Double

	scalarProduct(xs, xs)                     //> res8: Double = 111.69
	
	def isPrime(n: Int): Boolean = (2 to n/2).forall(n % _ != 0)
                                                  //> isPrime: (n: Int)Boolean
	isPrime(27)                               //> res9: Boolean = false
	
	isPrime(23)                               //> res10: Boolean = true
}