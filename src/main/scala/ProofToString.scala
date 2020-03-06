object ProofToString {
  def convert(proof: Proof): String =
    this.derivationToString(proof) + "\n\n" + this.currentProblemToString(proof)

  // Creates a String representation of the current Problem in the MacLogic style.
  def currentProblemToString(derivation: Proof): String = derivation match {
    case OngoingProof(problem, selectorStack) =>
      val selector = selectorStack.peek
      val currentProblem = problem.select(selector)
      val lhsString = currentProblem.sequent.lhs.foldLeft("")((acc, expr) => acc + "\n" + expr.toString)
      val rhsString = currentProblem.sequent.rhs.toString
      "Using:-" + lhsString + "\n\nDerive:-\n" + rhsString
    case ErrorProof(error, proof) => error.toString
    case FinishedProof(problem) => "Finished"
  }

  @scala.annotation.tailrec
  final def derivationToString(proof: Proof): String = proof match {
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
      case ClosedProblem(sequent) => "?  " + sequent.toString + "  \u25A0"
      case SplitProblem(sequent, tactic, _, subProblems) =>
        val tacticString = this.tacticToString(tactic, dashString)
        val subProblemsString = this.subProblemsToString(subProblems, currentSelector, selectorAcc, dashString)
        "?  " + sequent.toString + "\n-" + tacticString + "\n" + subProblemsString
    })
  }

  private def openProblemToString(sequent: Sequent, currSel: ProblemSelector, selAcc: ProblemSelector): String =
    "?  " + sequent.toString +
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
    dashString + "Using tactic for " + tactic.toString
}
