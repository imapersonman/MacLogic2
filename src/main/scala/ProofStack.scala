// Represents a Proof which keeps track of what sub-Problems are to be proved next.
sealed trait Proof {
  // Uses the given Tactic with the given Expr on the current problem in this Proof.
  def useTactic(tactic: Tactic, expr: Expr): Proof

  // Are all subProblems closed?
  def canClose: Boolean = this match {
    case OngoingProof(problem, selectorStack) => problem.isClosed
    case ErrorProof(error, proof) => proof.canClose
    case FinishedProof(problem) => true
  }

  // Returns this proof as finished if and only if every sub-Problem is closed.
  def finish: FinishedProof = this match {
    case OngoingProof(problem, _) =>
      if (problem.isClosed) FinishedProof(problem)
      else throw new IllegalArgumentException("Unable to finish proof due to OpenProblems")
    case ErrorProof(error, proof) => throw new UnsupportedOperationException("Cannot finish an ErrorProof")
    case FinishedProof(problem) => throw new UnsupportedOperationException("a FinishedProof is already finished")
  }

  // Returns the subProblem that useTactic will be acting on.
  def currentProblem: Problem = this match {
    case OngoingProof(problem, selectorStack) => problem.select(selectorStack.peek)
    case ErrorProof(_, proof) => proof.currentProblem
    case FinishedProof(_) => throw new IllegalArgumentException("FinishedProof does not have currentProblem")
  }
}

object Proof {
  // Creates a new Proof whose only Problem is an OpenProblem whose sequent is goal.
  def start(goal: Sequent): OngoingProof = OngoingProof(OpenProblem(goal))
}

// Represents a Proof that has not yet been completed, i.e. still has OpenProblems somewhere in its Problem.
case class OngoingProof(problem: Problem, selectorStack: ProblemSelectorStack) extends Proof {
  def useTactic(tactic: Tactic, expr: Expr): Proof = this.selectorStack match {
    case EmptyProblemSelectorStack => throw new IllegalStateException("No current selector for open problem")
    case ConsProblemSelectorStack(first, rest) =>
      val currentProblem = this.problem.select(first)
      // Assume the selector worked and (select should only throw Exceptions)
      val modified = currentProblem.useTactic(tactic, expr)
      this.errorOrUpdateProof(modified, first, rest)
  }

  // If successOrError is an Error, returns an ErrorProof with the given Error and this.problem.
  // Otherwise add successOrError's sub-Problems to the resulting OngoingProof and make its problem this.problem with
  // successOrError replacing the Problem associated with selector.
  private def errorOrUpdateProof(successOrError: ProblemOrError,
                                 selector: ProblemSelector,
                                 rest: ProblemSelectorStack): Proof = successOrError match {
    case error: Error => ErrorProof(error, this)
    case success: Problem => OngoingProof(
      this.problem.replace(selector, success),
      this.addSubProblems(success, selector, rest))
  }

  // Adds the ProblemSelectors associated with the sub-Problems of p to psStack, basing them off of psBase which should
  // be the ProblemSelector associated with p.
  private def addSubProblems(p: Problem,
                             psBase: ProblemSelector,
                             psStack: ProblemSelectorStack): ProblemSelectorStack = p match {
    // If p is a ClosedProblem, don't add anything to the stack.
    case ClosedProblem(_) => psStack
    // If p is an OpenProblem, add psBase to the stack so that it points to the p to be solved.
    case OpenProblem(_) => psStack.push(psBase)
    // If p is a SplitProblem, add its subProblem's ProblemSelectors to the stack.
    case SplitProblem(_, _, _, subProblems) =>
      val modSelectors = for (index <- subProblems.indices) yield psBase.addToEnd(index)
      // Older stack elements should be after newer stack elements.
      ProblemSelectorStack.mk(modSelectors).append(psStack)
  }
}

object OngoingProof {
  def apply(problem: Problem): OngoingProof = OngoingProof(problem, ProblemSelectorStack(EmptyProblemSelector))
}

// Represents a Proof in which an Error occurred.
case class ErrorProof(error: Error, proof: OngoingProof) extends Proof {
  override def useTactic(tactic: Tactic, expr: Expr): Proof = this.proof.useTactic(tactic, expr)
}

// Represents a Proof whose Problems are either SplitProblems or ClosedProblems.
case class FinishedProof(problem: Problem) extends Proof {
  override def useTactic(tactic: Tactic, expr: Expr): Proof =
    throw new UnsupportedOperationException("Cannot use Tactic on a FinishedProof")
}

// Represents a stack of Problems that is used to keep track of which Problem is currently being worked on.
sealed trait ProblemSelectorStack {
  // Pushes the given selector to the head of this ProblemSelectorStack.
  def push(selector: ProblemSelector): ProblemSelectorStack =
    ConsProblemSelectorStack(selector, this)

  // Peeks at the ProblemSelector at the top of this ProblemSelectorStack.
  def peek: ProblemSelector = this match {
    case EmptyProblemSelectorStack =>
      throw new UnsupportedOperationException("Cannot peek into EmptyProblemSelectorStack")
    case ConsProblemSelectorStack(first, _) => first
  }

  def append(psStack: ProblemSelectorStack): ProblemSelectorStack = this match {
    case EmptyProblemSelectorStack => psStack
    case ConsProblemSelectorStack(first, rest) =>
      ConsProblemSelectorStack(first, rest.append(psStack))
  }
}

object ProblemSelectorStack {
  def apply(selectors: ProblemSelector*): ProblemSelectorStack =
    this.applyAcc(selectors.reverse, EmptyProblemSelectorStack)

  def mk(selectors: Seq[ProblemSelector]): ProblemSelectorStack =
    applyAcc(selectors.reverse, EmptyProblemSelectorStack)

  @scala.annotation.tailrec
  def applyAcc(selectors: Seq[ProblemSelector], acc: ProblemSelectorStack): ProblemSelectorStack =
    if (selectors.isEmpty) acc
    else this.applyAcc(selectors.tail, ConsProblemSelectorStack(selectors.head, acc))
}

// Represents a ProblemStack that doesn't have any more Problems to work on.
case object EmptyProblemSelectorStack extends ProblemSelectorStack

// Represents a ProblemStack where there are still Problems to be worked on.
case class ConsProblemSelectorStack(first: ProblemSelector, rest: ProblemSelectorStack) extends ProblemSelectorStack
