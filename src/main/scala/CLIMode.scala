// Represents a mode of interaction for a command-line interface, specifically one for MacLogic's construction mode.
sealed trait CLIMode {
  // The CLIMode occurring after this one, depending on the given input String.
  def next(input: String): CLIMode

  // The string to display at the prompt to the user.
  def prompt: String

  // The String to display which represents this CLIMode.
  def output: String

  def updateUi(ui: MacLogicUi): Unit = ()

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
  override def output: String = "ERROR: " + this.message
}

// Represents the CLIMode which expects a list of premises to be inputted.
case object CLIExpectPremises extends CLIMode {
  def next(input: String): CLIMode = {
    ExprParser.parseList(input) match {
      case Left(error) => CLIError(error.message, this)
      case Right(premises) => CLIExpectConclusion(premises)
    }
  }

  override def updateUi(ui: MacLogicUi): Unit = ui.clear()

  override def prompt: String = "premises: "

  override def output: String = "Please enter a comma-separated list of premises"
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

  override def updateUi(ui: MacLogicUi): Unit = ui.clear()

  override def prompt: String = "conclusion: "

  override def output: String =
    "Premises: " + this.premises.mkString(", ") +
      "\nPlease enter conclusion"
}

// Represents a CLIMode that expects a tactic to be entered.
case class CLIExpectTactic(proof: Proof) extends CLIMode {
  def next(input: String): CLIMode = this.parseTactic(input) match {
    case Left(errorMessage) => CLIError(errorMessage, this)
    case Right(tactic) => CLIExpectExpr(this.proof, tactic)
  }

  override def prompt: String = "tactic: "

  override def updateUi(ui: MacLogicUi): Unit = {
    ui.problemTreeArea.text = ProofToString.derivationToString(this.proof)
    ui.currentProblemTextArea.text = ProofToString.currentProblemToString(this.proof)
  }

  override def output: String =
    "Now proving " + this.proof.currentProblem.sequent.toString +
      "\nEnter tactic"

  private def parseTactic(str: String): Either[String, Tactic] = str match {
    case "->I" => Right(ImpI)
    case "->E" => Right(ImpE)
    case "\u00acI" | "~I" | "-I" => Right(NotI)
    case "\u00acE" | "~E" | "-E" => Right(NotE)
    case "\u2227I" | "&I" => Right(AndI)
    case "\u2227" | "&E" => Right(AndE)
    case "\u2228I_left" | "\\/I_left" => Right(OrILeft)
    case "\u2228I_right" | "\\/I_right" => Right(OrIRight)
    case "\u2228E" | "\\/E" => Right(OrE)
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
  }

  override def prompt: String = "expression: "

  override def output: String = "Enter expression for tactic " + this.tactic.toString
}

// Represents the final mode of a CLI.
case class CLIFinished(proof: FinishedProof) extends CLIMode {
  override def next(input: String): CLIMode = this

  override def prompt: String = throw new UnsupportedOperationException("CLIQuit cannot be prompted")

  override def updateUi(ui: MacLogicUi): Unit = {
    ui.problemTreeArea.text = ProofToString.derivationToString(this.proof)
    ui.currentProblemTextArea.text = "Finished"
  }

  override def output: String = "Finished"
}
