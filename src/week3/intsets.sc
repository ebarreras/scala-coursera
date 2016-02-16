package week3

object intsets {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  EmptyIntSet.add(3).contains(3)                  //> res0: Boolean = true

  EmptyIntSet.add(4).add(5).add(6)                //> res1: week3.IntSet = [.4[.5[.6.]]]

  EmptyIntSet.union(EmptyIntSet.add(3))           //> res2: week3.IntSet = [.3.]

  EmptyIntSet.add(4).union(EmptyIntSet)           //> res3: week3.IntSet = [.4.]
}

abstract class IntSet {
  def add(x: Int): IntSet
  def contains(x: Int): Boolean
  def isEmpty(): Boolean
  def union(other: IntSet): IntSet
}

object EmptyIntSet extends IntSet {

  def contains(x: Int) = false
  def add(v: Int) = new NonEmptyIntSet(v, EmptyIntSet, EmptyIntSet)
  def isEmpty() = true
  def union(other: IntSet) = other
  override def toString() = "."
}

class NonEmptyIntSet(v: Int, l: IntSet, r: IntSet) extends IntSet {

  def value = v
  def left = l
  def right = r

  def contains(v: Int) =
    if (v == value) true else if (v < value) left.contains(v) else right.contains(v)

  def add(v: Int) =
    if (v == value) this else if (v < value) new NonEmptyIntSet(this.v, left.add(v), right) else new NonEmptyIntSet(this.value, left, right.add(v))

  def isEmpty() = false

  def union(other: IntSet) =
    left.union(right).union(other).add(value)

  override def toString() = "[" + left + value + right + "]"
}