import scala.util.{Failure, Success}

// Represents a mode of interaction for a command-line interface, specifically one for MacLogic's construction mode.
sealed trait CLIMode {
  // The CLIMode occurring after this one, depending on the given input String.
  def next(input: String): CLIMode

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
  override def updateUi(ui: MacLogicUi): Unit = ui.logToConsole("ERROR: " + this.message)
}

// Represents the CLIMode which expects a list of premises to be inputted.
case object CLIExpectPremises extends CLIMode {
  def next(input: String): CLIMode = {
    ExprParser.parseExprSeq(input) match {
      case Failure(exception) => CLIError(exception.getMessage, this)
      case Success(premises) => CLIExpectConclusion(premises)
    }
  }

  override def updateUi(ui: MacLogicUi): Unit = {
    ui.clear()
    ui.logToConsole("Please enter a comma-separated list of premises")
  }
}

// Represents a CLIMode that expects a single conclusion to be inputted.
case class CLIExpectConclusion(premises: Seq[Expr]) extends CLIMode {
  def next(input: String): CLIMode =
    if (input.isEmpty)
      CLIError("Conclusion must be non-empty", this)
    else ExprParser.parseExpr(input) match {
      case Failure(exception) => CLIError(exception.getMessage, this)
      case Success(conclusion) => CLIExpectTactic(Proof.start(Sequent(this.premises, conclusion)))
    }

  override def updateUi(ui: MacLogicUi): Unit = {
    ui.clear()
    ui.logToConsole("Premises: " + this.premises.mkString(", ") + "\nPlease enter conclusion")
  }
}

// Represents a CLIMode that expects a tactic to be entered.
case class CLIExpectTactic(proof: Proof) extends CLIMode {
  private val proofSystem = NK

  def next(input: String): CLIMode = this.proofSystem.parseTactic(input) match {
    case Failure(exception) => CLIError(exception.getMessage, this)
    case Success(tactic) => CLIExpectExpr(this.proof, tactic)
  }

  override def updateUi(ui: MacLogicUi): Unit = {
    ui.problemTreeArea.text = ProofToString.derivationToString(this.proof)
    ui.currentProblemTextArea.text = ProofToString.currentProblemToString(this.proof)
    ui.logToConsole("Now proving " + this.proof.currentProblem.sequent.toString + "\nEnter tactic")
  }
}

// Represents a CLIMode that expects and Expr to be entered that is then used on a Proof with a Tactic.
case class CLIExpectExpr(proof: Proof, tactic: Tactic) extends CLIMode {
  def next(input: String): CLIMode = ExprParser.parseExpr(input) match {
    case Failure(exception) => CLIError(exception.getMessage, this)
    case Success(expr) =>
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

  override def updateUi(ui: MacLogicUi): Unit = ui.logToConsole("Enter expression for tactic " + this.tactic.toString)
}

// Represents the final mode of a CLI.
case class CLIFinished(proof: FinishedProof) extends CLIMode {
  override def next(input: String): CLIMode = this

  override def updateUi(ui: MacLogicUi): Unit = {
    ui.problemTreeArea.text = ProofToString.derivationToString(this.proof)
    ui.currentProblemTextArea.text = "Finished"
    ui.logToConsole("Finished")
  }
}
