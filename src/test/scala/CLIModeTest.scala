import org.scalatest.FunSuite

class CLIModeTest extends FunSuite {
  private val A = SL("A")
  private val B = SL("B")
  private val C = SL("C")

  private val expectConclusionEmpty = CLIExpectConclusion(List())
  private val expectConclusionAandB = CLIExpectConclusion(List(And(A, B)))
  private val expectConclusionABC = CLIExpectConclusion(List(A, B, C))

  private val goalAandB = Sequent(Seq(And(A, B)), A)
  private val proofStart = OngoingProof.start(goalAandB)

  test("CLIExpectPremises.next") {
    assert(CLIExpectPremises.next("") == expectConclusionEmpty)
    assert(CLIExpectPremises.next("A & B") == expectConclusionAandB)
    assert(CLIExpectPremises.next("A, B, C") == expectConclusionABC)
  }

  test("CLIExpectConclusion.next") {
    assert(expectConclusionEmpty.next("") == CLIError("Conclusion must be non-empty", expectConclusionEmpty))
    assert(expectConclusionAandB.next("") == CLIError("Conclusion must be non-empty", expectConclusionAandB))
    assert(expectConclusionAandB.next("A") == CLIExpectTactic(proofStart))
  }
}
