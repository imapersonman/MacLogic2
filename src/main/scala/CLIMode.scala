import scala.util.{Failure, Success}

// Represents a mode of interaction for a command-line interface, specifically one for MacLogic's construction mode.
sealed trait CLIMode {
  // Respond to the goPressed event
  def goPressed(ui: MacLogicUi, specification: MacLogicStringSpecification): CLIMode = this

  // Handles the event according to the given String.
  def handleEvent(ui: MacLogicUi,
                  spec: MacLogicStringSpecification,
                  eventString: String): CLIMode = eventString match {
    case "Go" => this.goPressed(ui, spec)
    case _ => throw new MacLogicException("'" + eventString + "' not a recognized event")
  }

  // Called when this CLIMode is first switched to.
  def start(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = this

  // Is this a CLIFinished?
  def isFinished: Boolean = this match {
    case CLIFinished(_) => true
    case _ => false
  }
}

// Represents a CLIMode that presents an error to the user.
case class CLIError(message: String, nextMode: CLIMode) extends CLIMode {
  override def start(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = {
    ui.input.text = ""
    ui.logToConsole(this.message)
    this.nextMode.start(ui, spec)
  }
}

// Represents the CLIMode which expects a list of premises to be inputted.
case object CLIExpectPremises extends CLIMode {
  override def goPressed(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = {
    val input = ui.input.text
    Utils.parseList(spec.exprSpec.parse, input) match {
      case Failure(exception) => CLIError(exception.getMessage, this)
      case Success(premises) => CLIExpectConclusion(premises)
    }
  }

  override def start(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = {
    ui.input.text = ""
    ui.logToConsole("Please enter a comma-separated list of premises")
    this
  }
}

// Represents a CLIMode that expects a single conclusion to be inputted.
case class CLIExpectConclusion(premises: Seq[Prop]) extends CLIMode {
  override def goPressed(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = {
    val input = ui.input.text
    if (input.isEmpty)
      CLIError("Conclusion must be non-empty", this)
    else spec.exprSpec.parse(input) match {
      case Failure(exception) => CLIError(exception.getMessage, this)
      case Success(conclusion) => CLIExpectTactic(OngoingProof.start(Sequent(this.premises, conclusion)))
    }
  }

  override def start(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = {
    ui.input.text = ""
    ui.logToConsole("Premises: " + this.premises.map(spec.exprSpec.print).mkString(", "))
    ui.logToConsole("Please enter a conclusion")
    this
  }
}

// Represents a CLIMode that expects a tactic to be entered.
case class CLIExpectTactic(proof: Proof) extends CLIMode {
  override def goPressed(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = {
    val input = ui.input.text
    spec.tacticSpec.tacticParser(input) match {
      case Failure(exception) => CLIError(exception.getMessage, this)
      case Success(tactic) => CLIExpectExpr(this.proof, tactic)
    }
  }

  override def start(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = {
    ui.input.text = ""
    ui.logToConsole("Now proving " + spec.seqSpec.print(this.proof.currentProblem.sequent) + "\nEnter tactic")
    ui.problemTreeArea.text = spec.proofSpec.derivationToString(this.proof)
    ui.currentProblemTextArea.text = spec.proofSpec.currentProblemToString(this.proof.currentProblem)
    this
  }
}

// Represents a CLIMode that expects and Expr to be entered that is then used on a Proof with a Tactic.
case class CLIExpectExpr(proof: Proof, tactic: Tactic) extends CLIMode {
  override def goPressed(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = {
    val input = ui.input.text
    spec.exprSpec.parse(input) match {
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
  }

  override def start(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = {
    ui.input.text = ""
    ui.logToConsole("Enter expression for tactic " + spec.tacticSpec.tacticPrinter(this.tactic))
    this
  }
}

// Represents the final mode of a CLI.
case class CLIFinished(proof: FinishedProof) extends CLIMode {
  override def goPressed(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = this

  override def start(ui: MacLogicUi, spec: MacLogicStringSpecification): CLIMode = {
    ui.input.text = ""
    ui.problemTreeArea.text = spec.proofSpec.derivationToString(this.proof)
    ui.currentProblemTextArea.text = "Finished"
    ui.logToConsole("Finished")
    this
  }
}
