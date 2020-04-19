import scala.util.{Failure, Success, Try}

case class TacticsSpecification(tactics: Seq[Tactic],
                                tacticParser: String => Try[Tactic],
                                tacticPrinter: Tactic => String)

object DefaultTacticsSpecification extends TacticsSpecification(
  // List of Tactics
  Seq(AndI, AndE, ImpI, ImpE, NotI, NotE, OrILeft, OrIRight, DN, EFQ, Close),
  // Tactic Parser
  {
    case "\u2192" | "->I" => Success(ImpI)
    case "\u2192" | "->E" => Success(ImpE)
    case "\u00acI" | "~I" | "-I" => Success(NotI)
    case "\u00acE" | "~E" | "-E" => Success(NotE)
    case "\u2227I" | "/\\I" | "&I" => Success(AndI)
    case "\u2227E" | "/\\E" | "&E" => Success(AndE)
    case "\u2228I_left" | "\\/I_left" => Success(OrILeft)
    case "\u2228I_right" | "\\/I_right" => Success(OrIRight)
    case "\u2228E" | "\\/E" => Success(OrE)
    case "close" => Success(Close)
    case "DN" => Success(DN)
    case "EFQ" => Success(EFQ)
    case str => Failure(new TacticParserException(str))
  },
  // Tactic Printer
  {
    case AndI => "\u2227I"
    case AndE => "\u2227E"
    case ImpI => "\u2192I"
    case ImpE => "\u2192E"
    case NotI => "\u00acI"
    case NotE => "\u00acE"
    case OrILeft => "\u2228I_left"
    case OrIRight => "\u2228I_right"
    case OrE => "\u2228E"
    case DN => "DN"
    case EFQ => "EFQ"
    case Close => "Close"
  })
