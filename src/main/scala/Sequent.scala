case class Sequent(lhs: Seq[Prop], rhs: Prop) {
  // Adds the given sequence of Exprs to the lhs of this Sequent.
  def addToLhs(exprs: Prop*): Sequent = Sequent(exprs ++ this.lhs, this.rhs)

  // Removes the given sequence of Exprs from the lhs of this Sequent.
  def removeFromLhs(exprs: Prop*): Sequent = Sequent(this.lhs.diff(exprs), this.rhs)

  // Does the lhs of this sequent contain the given Expr?
  def lhsContains(expr: Prop): Boolean = this.lhs.contains(expr)

  // Replaces the rhs of this with the given Expr.
  def replaceRhs(expr: Prop): Sequent = Sequent(this.lhs, expr)

  // Does the rhs of this sequent contain the given Expr?
  def rhsContains(expr: Prop): Boolean = this.rhs == expr

  override def toString: String = this.lhs.mkString(", ") + " \u22A2 " + this.rhs.toString
}
