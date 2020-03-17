import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

trait CstParser[GenericExpr <: Expr] {
  def parseExpr(input: String): Try[GenericExpr]
  def parseExprSeq(input: String): Try[Seq[GenericExpr]]
}

object ExprParser extends RegexParsers with CstParser[Expr] {
  def sl: Parser[SL] = """[A-Z]+""".r ^^ { id => SL(id) }
  def not: Parser[Expr] = ("~" | "-") ~ factor ^^ { case _ ~ expr => Not(expr) }
  def absurd: Parser[Expr] = ("Absurd" | "\\F") ^^ (_ => Absurd)
  def nested: Parser[Expr] = "(" ~ expression ~ ")" ^^ { case _ ~ expr ~ _ => expr }
  def factor: Parser[Expr] = absurd | sl | not | nested
  def binOp: Parser[Expr] = makeBinOp("&", (lhs, rhs) => And(lhs, rhs)) |
      makeBinOp("\\/", (lhs, rhs) => Or(lhs, rhs)) |
      makeBinOp("->", (lhs, rhs) => Imp(lhs, rhs))
  def expression: Parser[Expr] = binOp | factor
  def exprList: Parser[List[Expr]] = (expression ~ ",".?).* ^^ { _.map(_._1) }

  private def makeBinOp(opString: String, constructor: (Expr, Expr) => Expr): Parser[Expr] =
    (factor ~ opString ~ factor) ^^ { case lhs ~ _ ~ rhs => constructor(lhs, rhs) }

  override def parseExprSeq(input: String): Try[Seq[Expr]] = parse(exprList, input) match {
    case Success(result, _) => scala.util.Success(result)
    case NoSuccess(msg, _) => scala.util.Failure(new IllegalArgumentException(msg))
  }

  override def parseExpr(input: String): Try[Expr] = parse(expression, input) match {
    case Success(result, _) => scala.util.Success(result)
    case NoSuccess(msg, _) => scala.util.Failure(new IllegalArgumentException(msg))
  }
}

//case class ParserError(message: String)
