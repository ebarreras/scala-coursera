package week6

object polynomials {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val chars = Map('A' -> 1, 'C' -> 3)             //> chars  : scala.collection.immutable.Map[Char,Int] = Map(A -> 1, C -> 3)

  chars.values                                    //> res0: Iterable[Int] = MapLike(1, 3)

  chars('A')                                      //> res1: Int = 1

  chars.get('B')                                  //> res2: Option[Int] = None

  val p = new Poly(3 -> 4.5, 1 -> 2, 0 -> 5)      //> p  : week6.Poly = 4.5^3 + 2.0^1 + 5.0

  val q = new Poly(4 -> 3, 3 -> 4.5, 1 -> -2, 0 -> 7)
                                                  //> q  : week6.Poly = 3.0^4 + 4.5^3 + -2.0^1 + 7.0

  p + q                                           //> res3: week6.Poly = 3.0^4 + 9.0^3 + 0.0^1 + 12.0
}

class Poly(val terms: Map[Int, Double]) {
  def this(pairs: (Int, Double)*) = this(pairs.toMap)

  def plusplus(other: Poly) = {
    val emptyMap = Map[Int, Double]()
    ((terms.keySet.union(other.terms.keySet) map (x => (x, terms.getOrElse(x, 0.0) + other.terms.getOrElse(x, 0.0))) filter ({ case (k, v) => v != 0.0 })) foldLeft Map[Int, Double]()) { case (m, (k, v)) => m + (k -> v) }
  }

  def plus(other: Poly) =
    terms.keySet.union(other.terms.keySet) map (x => (x, terms.getOrElse(x, 0.0) + other.terms.getOrElse(x, 0.0))) filter ({ case (k, v) => v != 0.0 }) toMap

  def addTerm(terms: Map[Int, Double], term: (Int, Double)) =
    terms + (term._1 -> (terms.getOrElse(term._1, 0.0) + term._2))

  def +(other: Poly) =
    new Poly((other.terms foldLeft terms)(addTerm))

  override def toString() = terms.toSeq.sortBy(_._1).reverse.map { case (k, v) => v + (if (k != 0) "^" + k else "") } mkString " + "

}