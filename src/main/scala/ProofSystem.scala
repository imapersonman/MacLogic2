import scala.util.{Failure, Success, Try}

trait ProofSystem[GenericExpr <: Expr, GenericTactic <: Tactic] {
  val tactics: Seq[GenericTactic]
  val parser: CstParser[GenericExpr]

  def parseTactic(input: String): Try[GenericTactic]
}

object NK extends ProofSystem[Expr, Tactic] {
  override val tactics: Seq[Tactic] = Seq(ImpI, ImpE, NotI, NotE, AndI, AndE, OrILeft, OrIRight, OrE, DN)
  override val parser: CstParser[Expr] = ExprParser

  override def parseTactic(input: String): Try[Tactic] = input match {
    case "->I" | "\u2192I" => Success(ImpI)
    case "->E" | "\u2192E" => Success(ImpE)
    case "-I" | "~I" => Success(NotI)
    case "-E" | "~E" => Success(NotE)
    case "&I" => Success(AndI)
    case "&E" => Success(AndE)
    case "\\/I_left" | "\u2228I_left" => Success(OrILeft)
    case "\\/I_right" | "\u2228I_right" => Success(OrIRight)
    case "\\/E" | "\u2228E" => Success(OrE)
    case "close" => Success(Close)
    case "DN" => Success(DN)
    case _ => Failure(new IllegalArgumentException("'" + input + "' is not a recognized Tactic"))
  }
}
