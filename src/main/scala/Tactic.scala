// A Tactic is a function with the signature
// (source: Sequent, expr: Expr) => Problem
// that satisfies a certain set of constraints.
trait Tactic {
  // Creates the problem resulting from applying this Tactic with the given Expr to the given Sequent.
  def apply(source: Sequent, expr: Prop): ProblemOrError
}

// Represents the Tactic that closes an OpenProblem whose sequent is trivially true.
object Close extends Tactic {
  override def apply(source: Sequent, expr: Prop): ProblemOrError =
    if (source.lhsContains(expr) && source.rhsContains(expr)) ClosedProblem(source)
    else Error("Given Expr must be present on both sides of Sequent to Close Problem")

  override def toString: String = "Close"
}

// Represents a Tactic used for introducing a connective.
trait IntroductionTactic extends Tactic {
  override def apply(source: Sequent, expr: Prop): ProblemOrError =
    if (source.rhsContains(expr))
      this.applySub(source, expr)
    else Error("Sequent must include given Expr in its rhs to apply I tactic")

  // To be implemented by extending Tactics.  Checking for existence of expr in the rhs of source
  // is done in apply so that Tactics overriding applySub don't have to worry about it.
  protected def applySub(source: Sequent, expr: Prop): ProblemOrError
}

// Represents a Tactic used for eliminating a connective.
trait EliminationTactic extends Tactic {
  override def apply(source: Sequent, expr: Prop): ProblemOrError =
    if (source.lhsContains(expr))
      this.applySub(source, expr)
    else Error("Sequent must include given Expr in its lhs to apply E tactic")

  // To be implemented by extending Types.  Checking for existence of expr in the lhs of source
  // is done in apply so that Tactics overriding applySub don't have to worry about it.
  protected def applySub(source: Sequent, expr: Prop): ProblemOrError
}

//  *L |- A
// -----------
//  *L |- ~~A
object DN extends Tactic {
  override def apply(source: Sequent, expr: Prop): ProblemOrError =
    if (source.rhs == expr)
      SplitProblem(source, DN, expr, Seq(OpenProblem(source.replaceRhs(Not(Not(expr))))))
    else
      Error("Given Expr must equal rhs to apply DN")

  override def toString: String = "DN"
}

//  *L, Absurd |- *R
// ----------------------
//  *L, *R, Absurd |- *R
object EFQ extends Tactic {
  override def apply(source: Sequent, expr: Prop): ProblemOrError =
    if (source.lhsContains(Absurd) && source.rhsContains(expr))
      SplitProblem(source, EFQ, expr, Seq(OpenProblem(source.addToLhs(expr))))
    else
      Error("Unable to apply EFQ (make errors better)")

  override def toString: String = "EFQ"
}

//  *L |- A & B
// -----------------------------
//  *L |- A     *L |- B
object AndI extends IntroductionTactic {
  override protected def applySub(source: Sequent, expr: Prop): ProblemOrError = expr match {
    case And(lhs, rhs) => SplitProblem(source, AndI, expr, Seq(
      OpenProblem(source.replaceRhs(lhs)),
      OpenProblem(source.replaceRhs(rhs))))
    case _ => Error("Main connective of \u2227I Expr must be \u2227")
  }

  override def toString: String = "\u2227I"
}

//  *L, A & B |- *R
// -----------------
//  *L, A, B |- *R
object AndE extends EliminationTactic {
  override protected def applySub(source: Sequent, expr: Prop): ProblemOrError = expr match {
    case And(lhs, rhs) =>
      SplitProblem(source, AndE, expr, Seq(OpenProblem(source.removeFromLhs(expr).addToLhs(lhs, rhs))))
    case _ => Error("Main connective of \u2227E Expr must be \u2227")
  }

  override def toString: String = "\u2227E"
}

//  *L |- A <-> B
// -------------------------------
//  *L |- A -> B     *L |- B -> A
object IffI extends IntroductionTactic {
  override protected def applySub(source: Sequent, expr: Prop): ProblemOrError = expr match {
    case Iff(lhs, rhs) => SplitProblem(source, IffI, expr, Seq(
      OpenProblem(source.replaceRhs(Imp(lhs, rhs))),
      OpenProblem(source.replaceRhs(Imp(rhs, lhs)))))
    case _ => Error("(IffI not applied correctly) make error handling better!")
  }
}

//  *L, A & B |- *R
// -----------------
//  *L, A, B |- *R
object IffE extends EliminationTactic {
  override protected def applySub(source: Sequent, expr: Prop): ProblemOrError = expr match {
    case Iff(lhs, rhs) => SplitProblem(source, IffE, expr, Seq(
        OpenProblem(source.removeFromLhs(expr).addToLhs(Imp(lhs, rhs), Imp(rhs, lhs)))))
    case _ => Error("(IffE not applied correctly) make error handling better!")
  }
}


//  *L |- A \/ B
// --------------
//  *L, A |- A
object OrILeft extends IntroductionTactic {
  override protected def applySub(source: Sequent, expr: Prop): ProblemOrError = expr match {
    case Or(lhs, _) => SplitProblem(source, OrILeft, expr, Seq(OpenProblem(source.replaceRhs(lhs))))
    case _ => Error("Main connective of \u2228I_left Expr must be \u2228")
  }

  override def toString: String = "\u2228I_left"
}

//  *L |- A \/ B
// --------------
//  *L |- B
object OrIRight extends IntroductionTactic {
  override protected def applySub(source: Sequent, expr: Prop): ProblemOrError = expr match {
    case Or(_, rhs) => SplitProblem(source, OrIRight, expr, Seq(OpenProblem(source.replaceRhs(rhs))))
    case _ => Error("Main connective of \u2228I_right Expr must be \u2228")
  }

  override def toString: String = "\u2228I_right"
}

//  *L, A \/ B |- *R
// -----------------------------
//  *L, A |- *R     *L, B |- *R
object OrE extends EliminationTactic {
  override protected def applySub(source: Sequent, expr: Prop): ProblemOrError = expr match {
    case Or(lhs, rhs) => SplitProblem(source, OrE, expr, Seq(
        OpenProblem(source.removeFromLhs(expr).addToLhs(lhs)),
        OpenProblem(source.removeFromLhs(expr).addToLhs(rhs))))
    case _ => Error("Main connective of \u2228E Expr must be \u2228")
  }

  override def toString: String = "\u2228E"
}

//  *L |- A -> B
// --------------
//  *L, A |- B
object ImpI extends IntroductionTactic {
  override protected def applySub(source: Sequent, expr: Prop): ProblemOrError = expr match {
    case Imp(lhs, rhs) => SplitProblem(source, ImpI, expr, Seq(
      OpenProblem(source.addToLhs(lhs).replaceRhs(rhs))))
    case _ => Error("Main connective of \u2192I must be \u2192")
  }

  override def toString: String = "\u2192I"
}

//  *L, A -> B |- *R
// -------------------------------
//  *L, A -> |- A     *L, B -> *R
object ImpE extends EliminationTactic {
  override protected def applySub(source: Sequent, expr: Prop): ProblemOrError = expr match {
    case Imp(lhs, rhs) => SplitProblem(source, ImpE, expr, Seq(
      OpenProblem(source.replaceRhs(lhs)),
      OpenProblem(source.removeFromLhs(expr).addToLhs(rhs))))
    case _ => Error("Main connective of \u2192E must be \u2192")
  }

  override def toString: String = "\u2192E"
}

//  *L |- ~A
// -----------------
//  *L, A |- Absurd
object NotI extends IntroductionTactic {
  override protected def applySub(source: Sequent, expr: Prop): ProblemOrError = expr match {
    case Not(sub) => SplitProblem(source, NotI, expr, Seq(OpenProblem(source.addToLhs(sub).replaceRhs(Absurd))))
    case _ => Error("Main connective of \u00acI must be \u00ac")
  }

  override def toString: String = "\u00acI"
}

//  *L, ~A |- *R
// ---------------------------------------
//  *L, ~A |- A      *L, Absurd |- *R
object NotE extends EliminationTactic {
  override protected def applySub(source: Sequent, expr: Prop): ProblemOrError = expr match {
    case Not(sub) => SplitProblem(source, NotE, expr, Seq(
      OpenProblem(source.replaceRhs(sub)),
      OpenProblem(source.removeFromLhs(expr).addToLhs(Absurd))))
    case _ => Error("Main connective of \u00acE must be \u00ac")
  }

  override def toString: String = "\u00acE"
}
