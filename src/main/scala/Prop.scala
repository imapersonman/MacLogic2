trait Expr

sealed trait Prop extends Expr

case class SL(id: String) extends Prop

case class Not(expr: Prop) extends Prop

case class And(lhs: Prop, rhs: Prop) extends Prop

case class Or(lhs: Prop, rhs: Prop) extends Prop

case class Imp(lhs: Prop, rhs: Prop) extends Prop

case object Absurd extends Prop
