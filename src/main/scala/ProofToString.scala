case class ProofToString(ts: TacticsSpecification,
                         es: ExprSpecification[Prop],
                         seqSpec: SequentSpecification) extends ProofSpecification {
  def convert(proof: Proof): String =
    this.derivationToString(proof) + "\n\n" + this.currentProblemToString(proof.currentProblem)

  // Creates a String representation of the current Problem in the MacLogic style.
  override def currentProblemToString(problem: Problem): String = {
    val lhsString = problem.sequent.lhs.foldLeft("")((acc, expr) => acc + "\n" + this.es.print(expr))
    val rhsString = this.es.print(problem.sequent.rhs)
    "Using:-" + lhsString + "\n\nDerive:-\n" + rhsString
  }

  @scala.annotation.tailrec
  override final def derivationToString(proof: Proof): String = proof match {
    case OngoingProof(problem, selectorStack) =>
      this.problemToString(problem, selectorStack.peek, ProblemSelector(), "")
    case ErrorProof(_, proof) => this.derivationToString(proof)
    case FinishedProof(problem) => this.problemToString(problem, NullProblemSelector, ProblemSelector(), "")
  }

  // Creates a String representation of a Problem like a derivation in the MacLogic style.
  // ACCUMULATOR selectorAcc: A ProblemSelector keeping track of where in the Problem we are so that we can check if we
  //                          are looking at the current problem.  Base case is ProblemSelector().
  // ACCUMULATOR dashString: A String of dashes to put in front of every line.  Used to separate sub-Problems from their
  //                         parents.  Base case is "".
  private def problemToString(problem: Problem,
                              currentSelector: ProblemSelector,
                              selectorAcc: ProblemSelector,
                              dashString: String): String = {
    dashString + (problem match {
      case OpenProblem(sequent) => this.openProblemToString(sequent, currentSelector, selectorAcc)
      case ClosedProblem(sequent) => "?  " + this.seqSpec.print(sequent) + "  \u25A0"
      case SplitProblem(sequent, tactic, _, subProblems) =>
        val tacticString = this.tacticToString(tactic, dashString)
        val subProblemsString = this.subProblemsToString(subProblems, currentSelector, selectorAcc, dashString)
        "?  " + this.seqSpec.print(sequent) + "\n-" + tacticString + "\n" + subProblemsString
    })
  }

  private def openProblemToString(sequent: Sequent, currSel: ProblemSelector, selAcc: ProblemSelector): String =
    "?  " + this.seqSpec.print(sequent) +
      (if (selAcc == currSel)
        " is the current problem"
      else "")

  private def subProblemsToString(problems: Seq[Problem],
                                  currentSelector: ProblemSelector,
                                  selectorAcc: ProblemSelector,
                                  dashString: String): String =
    problems.zipWithIndex.map(sub =>
      this.problemToString(
        sub._1,
        currentSelector,
        selectorAcc.addToEnd(sub._2),
        dashString + "-")).mkString("\n")

  private def tacticToString(tactic: Tactic, dashString: String): String =
    dashString + "Using tactic for " + this.ts.tacticPrinter(tactic)
}
