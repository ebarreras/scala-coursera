package week5

object mergesort {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def merge[T](l1: List[T], l2: List[T])(implicit ord: Ordering[T]): List[T] = (l1, l2) match {
  	case (_, Nil) => l1
  	case (Nil, _) => l2
  	case (x :: xs, y :: ys) => if (ord.lt(x, y)) x :: merge(xs, l2) else y :: merge(l1, ys)
  }                                               //> merge: [T](l1: List[T], l2: List[T])(implicit ord: Ordering[T])List[T]
  
  def msort[T](l: List[T])(implicit ord: Ordering[T]): List[T] = {
  	val n = l.length/2
  	if (n == 0) {
  		l
  	} else {
  		merge(msort(l.take(n)), msort(l.drop(n)))
  	}
	}                                         //> msort: [T](l: List[T])(implicit ord: Ordering[T])List[T]
	
	msort(2 :: 5 :: 4 :: 9 :: 6 :: 5 :: Nil)
                                                  //> res0: List[Int] = List(2, 4, 5, 5, 6, 9)
	
}