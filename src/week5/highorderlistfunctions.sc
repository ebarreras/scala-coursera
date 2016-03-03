package week5

object highorderlistfunctions {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def squareListPM(xs: List[Int]): List[Int] = xs match {
    case Nil     => Nil
    case y :: ys => y * y :: squareListPM(ys)
  }                                               //> squareListPM: (xs: List[Int])List[Int]

  def squareList(xs: List[Int]): List[Int] =
    xs map (x => x * x)                           //> squareList: (xs: List[Int])List[Int]

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil      => Nil
    case x :: xs1 => val (same, rest) = xs.span(x1 => x1 == x); same :: pack(rest)
  }                                               //> pack: [T](xs: List[T])List[List[T]]

  def encode[T](xs: List[T]): List[(T, Int)] = xs match {
    case Nil      => Nil
    case x :: xs1 => val (same, rest) = xs.span(x1 => x1 == x); (x, same.length) :: encode(rest)
  }                                               //> encode: [T](xs: List[T])List[(T, Int)]

  val numbers = 2 :: 4 :: 5 :: 9 :: 6 :: 5 :: Nil //> numbers  : List[Int] = List(2, 4, 5, 9, 6, 5)

  squareListPM(numbers)                           //> res0: List[Int] = List(4, 16, 25, 81, 36, 25)
  squareList(numbers)                             //> res1: List[Int] = List(4, 16, 25, 81, 36, 25)

  numbers.span(x => x % 2 == 0)                   //> res2: (List[Int], List[Int]) = (List(2, 4),List(5, 9, 6, 5))

  pack(List("a", "a", "a", "b", "c", "c", "a"))   //> res3: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a))
                                                  //| 
  encode(List("a", "a", "a", "b", "c", "c", "a")) //> res4: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))

  var l: List[Int] = Nil                          //> l  : List[Int] = List()

  l.foldLeft(0)(_ * _)                            //> res5: Int = 0

  def reduceRight[T](xs: List[T])(op: (T, T) => T): T = xs match {
    case Nil      => throw new UnsupportedOperationException
    case x :: Nil => x
    case x :: xs  => op(reduceRight(xs)(op), x)
  }                                               //> reduceRight: [T](xs: List[T])(op: (T, T) => T)T

  def foldRight[T, U](z: U, xs: List[T])(op: (U, T) => U): U = xs match {
    case Nil     => z
    case x :: xs => op(foldRight(z, xs)(op), x)
  }                                               //> foldRight: [T, U](z: U, xs: List[T])(op: (U, T) => U)U

  reduceRight(2 :: 4 :: 16 :: Nil)(_ / _)         //> res6: Int = 2

  foldRight("", 2 :: 4 :: 16 :: Nil)(_ + _)       //> res7: String = 1642

  def mapFun[T, U](xs: List[T])(f: T => U): List[U] =
    (xs foldRight List[U]())(f(_) :: _)           //> mapFun: [T, U](xs: List[T])(f: T => U)List[U]

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0)((x, acc) => acc + 1)         //> lengthFun: [T](xs: List[T])Int
    
    
  mapFun(2 :: 4 :: 16 :: Nil)(_ * 2)              //> res8: List[Int] = List(4, 8, 32)
  lengthFun(2 :: 4 :: 16 :: Nil)                  //> res9: Int = 3
}