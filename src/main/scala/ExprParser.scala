import scala.util.parsing.combinator.RegexParsers

object ExprParser extends RegexParsers {
  def notC: Parser[String] = "~" | "-" | "\u00ac"
  def andC: Parser[String] = "&" | "/\\" | "\u2227"
  def impC: Parser[String] = "->" | "\u2192"
  def orC: Parser[String]  = "\\/" | "\u2228"

  def sl: Parser[SL] = """[A-Z]+""".r ^^ { id => SL(id) }
  def absurd: Parser[Expr] = ("Absurd" | "\\F") ^^ (_ => Absurd)
  def nested: Parser[Expr] = "(" ~ expression ~ ")" ^^ { case _ ~ expr ~ _ => expr }
  def factor: Parser[Expr] = absurd | sl | not | nested
  def not: Parser[Expr] = notC ~ factor ^^ { case _ ~ expr => Not(expr) }
  def binOp: Parser[Expr] = makeBinOp(andC, (lhs, rhs) => And(lhs, rhs)) |
      makeBinOp(orC, (lhs, rhs) => Or(lhs, rhs)) |
      makeBinOp(impC, (lhs, rhs) => Imp(lhs, rhs))
  def expression: Parser[Expr] = binOp | factor
  def exprList: Parser[List[Expr]] = (expression ~ ",".?).* ^^ { _.map(_._1) }

  private def makeBinOp(op: Parser[String], constructor: (Expr, Expr) => Expr): Parser[Expr] =
    (factor ~ op ~ factor) ^^ { case lhs ~ _ ~ rhs => constructor(lhs, rhs) }

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
