package week4

object lists {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def insert(e: Int, l: List[Int]): List[Int] = l match {
    case List()  => List(e)
    case x :: xs => if (x <= e) x :: insert(e, xs) else e :: l
  }                                               //> insert: (e: Int, l: List[Int])List[Int]

  def isort(l: List[Int]): List[Int] = l match {
    case Nil      => l
    case x :: Nil => l
    case x :: xs  => insert(x, isort(xs))
  }                                               //> isort: (l: List[Int])List[Int]

  insert(5, List[Int]())                          //> res0: List[Int] = List(5)

  isort(2 :: 5 :: 4 :: 9 :: 6 :: 5 :: Nil)        //> res1: List[Int] = List(2, 4, 5, 5, 6, 9)

}