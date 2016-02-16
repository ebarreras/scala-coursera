package week3

object conslist {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  new Cons(1, new Nil[Int])                       //> res0: week3.Cons[Int] = [1, []]
  
  Lists.of(3).nth(-1)                             //> java.lang.IndexOutOfBoundsException
                                                  //| 	at week3.List$class.nth(week3.conslist.scala:17)
                                                  //| 	at week3.Nil.nth(week3.conslist.scala:27)
                                                  //| 	at week3.List$class.nth(week3.conslist.scala:18)
                                                  //| 	at week3.Cons.nth(week3.conslist.scala:21)
                                                  //| 	at week3.conslist$$anonfun$main$1.apply$mcV$sp(week3.conslist.scala:8)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at week3.conslist$.main(week3.conslist.scala:3)
                                                  //| 	at week3.conslist.main(week3.conslist.scala)
  
}

trait List[T] {
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
	def nth(i: Int): T =
		if (isEmpty) throw new IndexOutOfBoundsException else
		if (i == 0) head else tail.nth(i - 1)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	override def isEmpty = false
	
	override def toString = "[" + head + ", " + tail + "]"
}

class Nil[T] extends List[T] {
	override def isEmpty = true
	override def head = throw new NoSuchElementException("Nil.head")
	override def tail = throw new NoSuchElementException("Nil.tail")
	
	override def toString = "[]"
}

object Lists {

	def of[T](e: T): List[T] = new Cons[T](e, new Nil[T])
}