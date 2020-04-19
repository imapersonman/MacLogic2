import scala.util.parsing.combinator.RegexParsers

object ExprParser extends RegexParsers {
  def notC: Parser[String] = "~" | "-" | "\u00ac"
  def andC: Parser[String] = "&" | "/\\" | "\u2227"
  def impC: Parser[String] = "->" | "\u2192"
  def orC: Parser[String]  = "\\/" | "\u2228"

  def sl: Parser[SL] = """[A-Z]+""".r ^^ { id => SL(id) }
  def absurd: Parser[Prop] = ("Absurd" | "\\F") ^^ (_ => Absurd)
  def nested: Parser[Prop] = "(" ~ expression ~ ")" ^^ { case _ ~ expr ~ _ => expr }
  def factor: Parser[Prop] = absurd | sl | not | nested
  def not: Parser[Prop] = notC ~ factor ^^ { case _ ~ expr => Not(expr) }
  def binOp: Parser[Prop] = makeBinOp(andC, (lhs, rhs) => And(lhs, rhs)) |
      makeBinOp(orC, (lhs, rhs) => Or(lhs, rhs)) |
      makeBinOp(impC, (lhs, rhs) => Imp(lhs, rhs))
  def expression: Parser[Prop] = binOp | factor
  def exprList: Parser[List[Prop]] = (expression ~ ",".?).* ^^ { _.map(_._1) }

  private def makeBinOp(op: Parser[String], constructor: (Prop, Prop) => Prop): Parser[Prop] =
    (factor ~ op ~ factor) ^^ { case lhs ~ _ ~ rhs => constructor(lhs, rhs) }

  def parseList(input: String): Either[ParserError, List[Prop]] = parse(exprList, input) match {
    case Success(result, _) => Right(result)
    case NoSuccess(msg, _) => Left(ParserError(msg))
  }

  def parse(input: String): Either[ParserError, Prop] = parse(expression, input) match {
    case Success(result, _) => Right(result)
    case NoSuccess(msg, _) => Left(ParserError(msg))
  }
}

case class ParserError(message: String)
