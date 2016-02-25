package week4

object lists {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def insert(e: Int, l: List[Int]): List[Int] = l match {
    case List()  => List(e)
    case x :: xs => if (x <= e) x :: insert(e, xs) else e :: l
  }                                               //> insert: (e: Int, l: List[Int])List[Int]

  def concat[T](l1: List[T], l2: List[T]): List[T] = l1 match {
    case Nil     => l2
    case x :: xs => x :: concat(xs, l2)
  }                                               //> concat: [T](l1: List[T], l2: List[T])List[T]

  def init(l: List[Int]): List[Int] = l match {
    case Nil      => throw new IllegalStateException
    case x :: Nil => Nil
    case x :: xs  => x :: init(xs)
  }                                               //> init: (l: List[Int])List[Int]

  def last(l: List[Int]): Int = l match {
    case Nil      => throw new NoSuchElementException
    case x :: Nil => x
    case x :: xs  => last(xs)
  }                                               //> last: (l: List[Int])Int

  def reverse(l: List[Int]): List[Int] = l match {
    case Nil      => l
    case x :: Nil => l
    case x :: xs  => concat(reverse(xs), x :: Nil)
  }                                               //> reverse: (l: List[Int])List[Int]

  def isort(l: List[Int]): List[Int] = l match {
    case Nil      => l
    case x :: Nil => l
    case x :: xs  => insert(x, isort(xs))
  }                                               //> isort: (l: List[Int])List[Int]

  def removeAt[T](n: Int, l: List[T]): List[T] = l match {
    case Nil     => throw new NoSuchElementException
    case x :: xs => if (n == 0) xs else x :: removeAt(n - 1, xs)
  }                                               //> removeAt: [T](n: Int, l: List[T])List[T]

  def flatten(l: List[Any]): List[Any] = l match {
    case Nil                   => Nil
    case (xs: List[Any]) :: ys => flatten(xs) ::: flatten(ys)
    case x :: xs               => x :: flatten(xs)
  }                                               //> flatten: (l: List[Any])List[Any]

  insert(5, List[Int]())                          //> res0: List[Int] = List(5)

  isort(2 :: 5 :: 4 :: 9 :: 6 :: 5 :: Nil)        //> res1: List[Int] = List(2, 4, 5, 5, 6, 9)

  last(2 :: 5 :: 4 :: 9 :: 6 :: 5 :: Nil)         //> res2: Int = 5

  init(2 :: 5 :: 4 :: 9 :: 6 :: 5 :: Nil)         //> res3: List[Int] = List(2, 5, 4, 9, 6)

  concat(2 :: 5 :: 4 :: 9 :: 6 :: 5 :: Nil, Nil)  //> res4: List[Int] = List(2, 5, 4, 9, 6, 5)
  concat(Nil, 2 :: 5 :: 4 :: 9 :: 6 :: 5 :: Nil)  //> res5: List[Int] = List(2, 5, 4, 9, 6, 5)
  concat(1 :: Nil, 2 :: 5 :: 4 :: 9 :: 6 :: 5 :: Nil)
                                                  //> res6: List[Int] = List(1, 2, 5, 4, 9, 6, 5)
  concat(2 :: 5 :: 4 :: 9 :: 6 :: 5 :: Nil, 1 :: Nil)
                                                  //> res7: List[Int] = List(2, 5, 4, 9, 6, 5, 1)
  concat(8 :: 5 :: 4 :: Nil, 6 :: 9 :: 6 :: 5 :: Nil)
                                                  //> res8: List[Int] = List(8, 5, 4, 6, 9, 6, 5)

  reverse(2 :: 5 :: 4 :: 9 :: 6 :: 5 :: Nil)      //> res9: List[Int] = List(5, 6, 9, 4, 5, 2)

  removeAt(3, 2 :: 5 :: 4 :: 9 :: 6 :: 5 :: Nil)  //> res10: List[Int] = List(2, 5, 4, 6, 5)

  flatten(List(List(), List(2), List(), List(List(3, 8), 9)))
                                                  //> res11: List[Any] = List(2, 3, 8, 9)

  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res12: List[Any] = List(1, 1, 2, 3, 5, 8)
}