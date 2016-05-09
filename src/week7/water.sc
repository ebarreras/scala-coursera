package week7

object water {
  println("Welcome to the Scala worksheet")
  
  def makeGlasses(sizes: Seq[Int]) = new Glasses((sizes map ((_, 0))).toVector)
  
  val glasses1 = makeGlasses(List(9,4))
  val glasses2 = makeGlasses(List(9,4))
  
  glasses1.equals(glasses2)
  glasses1.get(1)
 	glasses1.hasGlassWith(3)
 	glasses1.hasGlassWith(5)
 	glasses1.fill(1).pour(1, 0).canPour(1, 0)
 	
 	def how(glasses: Glasses, target: Int): Option[List[String]] = {
 		def iter(state: Glasses, past: Set[Glasses], actions: List[String]): Option[List[String]] = {
 			if (state.hasGlassWith(target)) Some(actions)
 			else if (past.contains(state)) None
 			else {
 				val byFilling = (0 until glasses.count) filter (!state.isFull(_)) map (i => {val newState = state.fill(i); iter(newState, past + state, ("fill " + i + ": " + newState) :: actions)}) find ( _.isDefined )
 				
 				lazy val byPouring = (for {
 					i <- 0 until glasses.count
 					j <- 0 until glasses.count
 					if i != j && state.canPour(i, j)
 				} yield {val newState = state.pour(i, j); iter(newState, past + state, ("pour " + i + " into " + j + ": " + newState) :: actions)}) find ( _.isDefined )
 				
 				lazy val byThrowingAway = (0 until glasses.count) filter (!state.isEmpty(_)) map (i => {val newState = state.toSink(i); iter(newState, past + state, ("throw away " + i + ": " + newState) :: actions)}) find ( _.isDefined )
 				
 				byFilling.orElse(byPouring).orElse(byThrowingAway)
 			}
 		}
 		
		val past: Set[Glasses] = Set()
		iter(glasses, past, Nil).map(_.reverse)
	}
	
	how(glasses1, 6) map (_  mkString "\n")
	
}

class Glasses(val glasses: Vector[(Int, Int)]) {

	def hasGlassWith(content: Int) = glasses exists {case (_, c) => c == content}
	
	def count = glasses.length

	def canPour(from: Int, to: Int) = !isEmpty(from) && !isFull(to)

  def pour(from: Int, to: Int) = {
    if (isEmpty(from)) throw new UnsupportedOperationException("does not make sense to pour empty glass")
    if (isFull(to)) throw new UnsupportedOperationException("can't pour into full glass")
    val amount = Math.min(contents(from), leftUnfilled(to))
  	new Glasses(glasses.updated(from, (capacity(from), contents(from) - amount)).updated(to, (capacity(to), contents(to) + amount)))
  }

  def toSink(i: Int) = new Glasses(glasses.updated(i, (capacity(i),0)))
  
  def fill(i: Int) = new Glasses(glasses.updated(i, (capacity(i), capacity(i))))

	def get(i: Int) = glasses(i)
	
	def isEmpty(i: Int) = glasses(i)._2 == 0
	
	def isFull(i: Int) = glasses(i)._1 == glasses(i)._2
	
	def capacity(i: Int) = glasses(i)._1
	
	def contents(i: Int) = glasses(i)._2
	
	def leftUnfilled(i: Int) = glasses(i)._1 - glasses(i)._2
	
	override def toString() = glasses.toString
	
	override def equals(that: Any): Boolean = that match {
		case that: Glasses => glasses.equals(that.glasses)
		case _ => false
	}
	
	override def hashCode(): Int = glasses.hashCode
		
}