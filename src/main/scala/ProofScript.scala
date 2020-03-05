// Represents a Proof/Problem laid out in the style of Forbes.
case class ProofScript(goal: Sequent, lines: Seq[Line])

object ProofScript {
  def fromProblem(problem: Problem): ProofScript = ???
}

// Represents the line of a ProofScript.
case class Line(dependencies: Seq[LineRef], ref: LineRef, derived: Expr, reason: Reason)

// Represents a reference associated with a Line in a ProofScript.
case class LineRef(id: Int)

// Represents a Line's justification in a ProofScript.
case class Reason(dependencies: Seq[LineRef])

object Reason {
  def assume: Reason = Reason(Seq())
  def notE(not: LineRef, other: LineRef): Reason = Reason(Seq(not, other))
  def notI(assume: LineRef, absurd: LineRef): Reason = Reason(Seq(assume, absurd))
  def orE(or: LineRef, ant1: LineRef, cons1: LineRef, ant2: LineRef, cons2: LineRef): Reason =
    Reason(Seq(or, ant1, cons1, ant2, cons2))
  def impI(ant: LineRef, cons: LineRef): Reason = Reason(Seq(ant, cons))
}

object ReasonExamples {
  val assume: Reason = Reason.assume
  val notE1: Reason = Reason.notE(LineRef(1), LineRef(4))
  val notE2: Reason = Reason.notI(LineRef(2), LineRef(6))
  val orE1: Reason = Reason.orE(LineRef(3), LineRef(4), LineRef(5), LineRef(6), LineRef(7))
  val notI1: Reason = Reason.notI(LineRef(3), LineRef(8))
  val impI1: Reason = Reason.impI(LineRef(2), LineRef(9))
}

object LineExamples {
  private val A = SL("A")
  private val B = SL("B")

  val line1_1: Line = Line(Seq(LineRef(1)), LineRef(1), Not(A), Reason.assume)
  val line1_2: Line = Line(Seq(LineRef(2)), LineRef(2), Not(B), Reason.assume)
  val line1_3: Line = Line(Seq(LineRef(3)), LineRef(3), Or(A, B), Reason.assume)
  val line1_4: Line = Line(Seq(LineRef(4)), LineRef(4), A, Reason.assume)
  val line1_5: Line = Line(Seq(LineRef(1), LineRef(4)), LineRef(5), Absurd, ReasonExamples.notE1)
  val line1_6: Line = Line(Seq(LineRef(6)), LineRef(6), B, Reason.assume)
  val line1_7: Line = Line(Seq(LineRef(2), LineRef(6)), LineRef(7), Absurd, ReasonExamples.notE2)
  val line1_8: Line = Line(Seq(LineRef(1), LineRef(2), LineRef(3)), LineRef(8), Absurd, ReasonExamples.orE1)
  val line1_9: Line = Line(Seq(LineRef(1), LineRef(2)), LineRef(9), Not(Or(A, B)), ReasonExamples.notI1)
  val line1_10: Line = Line(Seq(), LineRef(10), Imp(Not(B), Not(Or(A, B))), ReasonExamples.impI1)

  val line2_1: Line = Line(Seq(LineRef(1)), LineRef(1), A, Reason.assume)

  val line3_1: Line = line2_1
  val line3_2: Line = Line(Seq(LineRef(2)), LineRef(2), B, Reason.assume)
}

object ProofScriptExamples {
  private val A = SL("A")
  private val B = SL("B")

  val goal1: Sequent = Sequent(Seq(Not(A)), Imp(Not(B), Not(Or(A, B))))
  val proofScript1: ProofScript = ProofScript(goal1, Seq(
    LineExamples.line1_1, LineExamples.line1_2, LineExamples.line1_3, LineExamples.line1_4, LineExamples.line1_5,
    LineExamples.line1_6, LineExamples.line1_7, LineExamples.line1_8, LineExamples.line1_9))
  val goal2: Sequent = Sequent(Seq(A), A)
  val proofScript2: ProofScript = ProofScript(goal2, Seq(LineExamples.line2_1))
  val goal3: Sequent = Sequent(Seq(A, B), A)
  val proofScript3: ProofScript = ProofScript(goal3, Seq(LineExamples.line3_1, LineExamples.line3_2))
}
