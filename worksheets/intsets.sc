object intsets {
  val t1 = new NonEmpty(3, Empty, Empty)          //> t1  : NonEmpty = {.3.}
  val t2 = t1 incl 4                              //> t2  : IntSet = {.3{.4.}}
  val t3 = t2 union Empty                         //> t3  : IntSet = {{.3.}4.}
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  override def toString = "."
  def union(other: IntSet): IntSet = other
}

class NonEmpty(value: Int, left: IntSet, right: IntSet) extends IntSet {
  def incl(x: Int): IntSet = {
    if (x < this.value)
      new NonEmpty(this.value, left.incl(x), right)
    else if (x > this.value)
      new NonEmpty(this.value, left, right.incl(x))
    else this
  }

  def contains(x: Int): Boolean = {
    if (x < this.value)
      left.contains(x)
    else if (x > this.value)
      right.contains(x)
    else true
  }

  override def toString = "{" + left + value + right + "}"

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl value
}