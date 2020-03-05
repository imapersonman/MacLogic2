import scala.io.StdIn.readLine

object CLI extends App {
  var mode: CLIMode = CLIExpectPremises
  while (!mode.isFinished) {
    mode.display()
    val input = readLine(mode.prompt)
    mode = mode.next(input)
  }
  mode.display()
}

// Represents a mode of interaction for a command-line interface, specifically one for MacLogic's construction mode.
sealed trait CLIMode {
  // The CLIMode occurring after this one, depending on the given input String.
  def next(input: String): CLIMode

  // The string to display at the prompt to the user.
  def prompt: String

  // Does some side-effect stuff to show the mode to the user, or not in certain cases.
  def display(): Unit

  // Is this a CLIFinished?
  def isFinished: Boolean = this match {
    case CLIFinished(_) => true
    case _ => false
  }
}

// Represents a CLIMode that presents an error to the user.
case class CLIError(message: String, nextMode: CLIMode) extends CLIMode {
  override def next(input: String): CLIMode = this.nextMode
  override def prompt: String = "Enter anything to proceed: "
  override def display(): Unit = println("ERROR: " + this.message)
}

// Represents the CLIMode which expects a list of premises to be inputted.
case object CLIExpectPremises extends CLIMode {
  def next(input: String): CLIMode = {
    ExprParser.parseList(input) match {
      case Left(error) => CLIError(error.message, this)
      case Right(premises) => CLIExpectConclusion(premises)
    }
  }

  override def prompt: String = "premises: "

  override def display(): Unit =
    println("Please enter a comma-separated list of premises, then a conclusion as prompted")
}

// Represents a CLIMode that expects a single conclusion to be inputted.
case class CLIExpectConclusion(premises: List[Expr]) extends CLIMode {
  def next(input: String): CLIMode =
    if (input.isEmpty)
      CLIError("Conclusion must be non-empty", this)
    else ExprParser.parse(input) match {
      case Left(error) => CLIError(error.message, this)
      case Right(conclusion) => CLIExpectTactic(OngoingProof.start(Sequent(this.premises, conclusion)))
    }

  override def prompt: String = "conclusion: "

  override def display(): Unit = ()
}

// Represents a CLIMode that expects a tactic to be entered.
case class CLIExpectTactic(proof: Proof) extends CLIMode {
  def next(input: String): CLIMode = this.parseTactic(input) match {
    case Left(errorMessage) => CLIError(errorMessage, this)
    case Right(tactic) => CLIExpectExpr(this.proof, tactic)
  }

  override def prompt: String = "tactic: "

  override def display(): Unit =
    println(ProofToString.convert(this.proof) + "\n")

  private def parseTactic(str: String): Either[String, Tactic] = str match {
    case "->I" => Right(ImpI)
    case "->E" => Right(ImpE)
    case "~I" | "-I" => Right(NotI)
    case "~E" | "-E" => Right(NotE)
    case "&I" => Right(AndI)
    case "&E" => Right(AndE)
    case "\\/Il" => Right(OrILeft)
    case "\\/Ir" => Right(OrIRight)
    case "\\/E" => Right(OrE)
    case "close" => Right(Close)
    case "DN" => Right(DN)
    case _ => Left("'" + str + "' is not a recognized Tactic")
  }
}

// Represents a CLIMode that expects and Expr to be entered that is then used on a Proof with a Tactic.
case class CLIExpectExpr(proof: Proof, tactic: Tactic) extends CLIMode {
  def next(input: String): CLIMode = ExprParser.parse(input) match {
    case Left(error) => CLIError(error.message, this)
    case Right(expr) =>
      val modProof = this.proof.useTactic(this.tactic, expr)
      modProof match {
        case OngoingProof(_, _) =>
          if (modProof.canClose) CLIFinished(modProof.finish)
          else CLIExpectTactic(modProof)
        case ErrorProof(error, _) => CLIError(error.message, this)
        case FinishedProof(_) => throw new UnsupportedOperationException(
            "Currently not handling case where FinishedProof is returned from useTactic")
      }
      if (modProof.canClose)
        CLIFinished(modProof.finish)
      else
        CLIExpectTactic(modProof)
  }

  override def prompt: String = "expression: "

  override def display(): Unit = ()
}

// Represents the final mode of a CLI.
case class CLIFinished(proof: FinishedProof) extends CLIMode {
  override def next(input: String): CLIMode = throw new UnsupportedOperationException("CLIQuit does not have a next")

  override def prompt: String = throw new UnsupportedOperationException("CLIQuit cannot be prompted")

  override def display(): Unit =
    println(ProofToString.convert(this.proof))
}
