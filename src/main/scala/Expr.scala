sealed trait Expr

case class SL(id: String) extends Expr {
  override def toString: String = id.toString
}

case class Not(expr: Expr) extends Expr {
  override def toString: String = "(~ " + this.expr.toString + ")"
}

case class And(lhs: Expr, rhs: Expr) extends Expr {
  override def toString: String = "(" + this.lhs.toString + " & " + this.rhs.toString + ")"
}

case class Or(lhs: Expr, rhs: Expr) extends Expr {
  override def toString: String = "(" + this.lhs.toString + " \\/ " + this.rhs.toString + ")"
}

case class Imp(lhs: Expr, rhs: Expr) extends Expr {
  override def toString: String = "(" + this.lhs.toString + " -> " + this.rhs.toString + ")"
}

case object Absurd extends Expr {
  override def toString: String = "\u22A5"
}
