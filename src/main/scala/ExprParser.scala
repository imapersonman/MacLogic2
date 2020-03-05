import scala.util.parsing.combinator.RegexParsers

object ExprParser extends RegexParsers {
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

  def parseList(input: String): Either[ParserError, List[Expr]] = parse(exprList, input) match {
    case Success(result, _) => Right(result)
    case NoSuccess(msg, _) => Left(ParserError(msg))
  }

  def parse(input: String): Either[ParserError, Expr] = parse(expression, input) match {
    case Success(result, _) => Right(result)
    case NoSuccess(msg, _) => Left(ParserError(msg))
  }
}

case class ParserError(message: String)
