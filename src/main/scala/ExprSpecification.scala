import scala.util.{Failure, Success, Try}

trait ExprSpecification[SubExpr <: Expr] {
  def parse(str: String): Try[SubExpr]
  def print(expr: SubExpr): String
}

object DefaultExprSpecification extends ExprSpecification[Prop]() {
  override def parse(str: String): Try[Prop] = ExprParser.parse(str) match {
    case Left(_) => Failure(new ExprParserException(str))
    case Right(expr) => Success(expr)
  }

  override def print(expression: Prop): String = expression match {
    case SL(id) => id
    case Not(expr) => "¬" + this.print(expr)
    case And(lhs, rhs) => "(" + print(lhs) + "∧" + print(rhs) + ")"
    case Or(lhs, rhs) => "(" + print(lhs) + "∨" + print(rhs) + ")"
    case Imp(lhs, rhs) => "(" + print(lhs) + "→" + print(rhs) + ")"
    case Absurd => "⊥"
  }
}
