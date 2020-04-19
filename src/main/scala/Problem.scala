// Represents the result of applying a Tactic to a Sequent.
sealed trait ProblemOrError

// Represents an error that can occur when trying to apply a Tactic to a Sequent.
case class Error(message: String) extends ProblemOrError

// Represents the partial or completed solution to a Sequent.
// TODO: Rename to something other than Problem (Solution?  Proof?  Derivation?)
sealed abstract class Problem(val sequent: Sequent) extends ProblemOrError {
  // Creates a new problem using the given Tactic and Expr.
  def useTactic(tactic: Tactic, expr: Prop): ProblemOrError = this match {
    case OpenProblem(sequent) => tactic.apply(sequent, expr)
    case ClosedProblem(_) => Error("Cannot use tactic on a ClosedProblem")
    case SplitProblem(_, _, _, _) => Error("Cannot use tactic on a SplitProblem")
  }

  // Finds and returns the Problem associated with the given ProblemSelector.
  def select(selector: ProblemSelector): Problem = selector match {
    case EmptyProblemSelector => this
    case ConsProblemSelector(index, rest) => this match {
      case SplitProblem(_, _, _, subProblems) =>
        if (index < 0 || index >= subProblems.length)
          throw new IllegalArgumentException("ProblemSelector out-of-bounds")
        else subProblems(index).select(rest)
      case _ => throw new IllegalArgumentException("ProblemSelector ")
    }
    case NullProblemSelector =>
      throw new IllegalArgumentException("Using NullProblemSelector (which shouldn't be a thing")
  }

  // Replaces the Problem associated with the given ProblemSelector with the given Problem.
  // TODO: REFACTOR WITH SELECT
  def replace(selector: ProblemSelector, problem: Problem): Problem = selector match {
    case EmptyProblemSelector => problem
    case ConsProblemSelector(index, rest) => this match {
      case SplitProblem(sequent, tactic, expr, subProblems) =>
        if (index < 0 || index >= subProblems.length)
          throw new IllegalArgumentException("ProblemSelector out-of-bounds")
        else {
          val replacement = subProblems(index).replace(rest, problem)
          SplitProblem(sequent, tactic, expr, subProblems.patch(index, Seq(replacement), 1))
        }
      case _ => throw new IllegalArgumentException("ProblemSelector under-selects")
    }
    case NullProblemSelector =>
      throw new IllegalArgumentException("NullProblemSelector (shouldn't be a thing)")
  }

  // Are all of this Problem's subProblems closed?
  def isClosed: Boolean = this match {
    case OpenProblem(_) => false
    case ClosedProblem(_) => true
    case SplitProblem(_, _, _, subProblems) => subProblems.forall(problem => problem.isClosed)
  }
}

// Represents a Problem that a Tactic can be applied to.
case class OpenProblem(override val sequent: Sequent) extends Problem(sequent)

// Represents a Problem resulting from the Close Tactic.
case class ClosedProblem(override val sequent: Sequent) extends Problem(sequent)

// Represents a Problem that has been split up into more than one sub-Problem.
case class SplitProblem(override val sequent: Sequent, tactic: Tactic, expr: Prop, subProblems: Seq[Problem])
  extends Problem(sequent)
