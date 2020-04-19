case class SequentSpecification(exprSpec: ExprSpecification[Prop]) {
  def print(sequent: Sequent): String =
    sequent.lhs.map(this.exprSpec.print).mkString(", ") + " ⊢ " + this.exprSpec.print(sequent.rhs)
}
