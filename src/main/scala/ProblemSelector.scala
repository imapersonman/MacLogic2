// Represents an entity used to "select" a Problem from a tree of Problems.
sealed trait ProblemSelector {
  def addToEnd(index: Int): ProblemSelector = this match {
    case EmptyProblemSelector => ProblemSelector(index)
    case ConsProblemSelector(first, rest) => ConsProblemSelector(first, rest.addToEnd(index))
    case NullProblemSelector =>
      throw new UnsupportedOperationException("Cannot add to the end of a NullProblemSelector")
  }

  def length: Int = this match {
    case EmptyProblemSelector => 0
    case ConsProblemSelector(_, rest) => 1 + rest.length
    case NullProblemSelector => throw new UnsupportedOperationException("NullProblemSelector has no length")
  }
}

// Represents a ProblemSelector that cannot be used to select anything.
case object NullProblemSelector extends ProblemSelector

// Represents a ProblemSelector that has found the Problem that it is selecting.
case object EmptyProblemSelector extends ProblemSelector

// Represents a ProblemSelector that hasn't yet found the Problem that it is selecting, using first to decide which
// branch of the Problem tree to continue searching down and rest to guide the rest of the search.
case class ConsProblemSelector(first: Int, rest: ProblemSelector) extends ProblemSelector

object ProblemSelector {
  // Creates a ProblemSelector that's made up of values, where the first element in values is the first element of the
  // returned ProblemSelector.
  def apply(values: Int*): ProblemSelector = this.applyAcc(values.reverse, EmptyProblemSelector)

  // Creates a ProblemSelector that's made up of values, where the first element in values is the last element of the
  // returned ProblemSelector.  Only called by apply.
  // ACCUMULATOR: A ProblemSelector whose base case is an EmptyProblemSelector.
  @scala.annotation.tailrec
  protected def applyAcc(values: Seq[Int], acc: ProblemSelector): ProblemSelector =
    if (values.isEmpty) acc
    else this.applyAcc(values.tail, ConsProblemSelector(values.head, acc))
}
