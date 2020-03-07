import org.scalatest.FunSuite

class ProofToStringTest extends FunSuite {
  private val A = SL("A")
  private val B = SL("B")
  private val C = SL("C")

  test("ProofToString derivationToString and currentProblemToString") {
    val goal = Sequent(Seq(And(A, And(B, C))), And(C, And(B, A)))
    val proof1 = OngoingProof.start(goal)
    assert(
      "?  (A & (B & C)) \u22A2 (C & (B & A)) is the current problem" ==
      ProofToString.derivationToString(proof1))
    assert(
      "Using:-" +
        "\n(A & (B & C))" +
        "\n" +
        "\nDerive:-" +
        "\n(C & (B & A))" ==
      ProofToString.currentProblemToString(proof1))

    val proof2 = proof1.useTactic(AndE, And(A, And(B, C)))
    assert(
      "?  (A & (B & C)) \u22A2 (C & (B & A))" +
        "\n-Using tactic for &E" +
        "\n-?  A, (B & C) \u22A2 (C & (B & A)) is the current problem" ==
      ProofToString.derivationToString(proof2))
    assert(
      "Using:-" +
        "\nA" +
        "\n(B & C)" +
        "\n" +
        "\nDerive:-" +
        "\n(C & (B & A))" ==
      ProofToString.currentProblemToString(proof2))

    val proof3 = proof2.useTactic(AndE, And(B, C))
    assert(
      "?  (A & (B & C)) \u22A2 (C & (B & A))" +
        "\n-Using tactic for &E" +
        "\n-?  A, (B & C) \u22A2 (C & (B & A))" +
        "\n--Using tactic for &E" +
        "\n--?  B, C, A \u22A2 (C & (B & A)) is the current problem" ==
      ProofToString.derivationToString(proof3))
    assert(
      "Using:-" +
        "\nB" +
        "\nC" +
        "\nA" +
        "\n" +
        "\nDerive:-" +
        "\n(C & (B & A))" ==
      ProofToString.currentProblemToString(proof3))


    val proof4 = proof3.useTactic(AndI, And(C, And(B, A))).useTactic(Close, C)
    assert(
      "?  (A & (B & C)) \u22A2 (C & (B & A))" +
        "\n-Using tactic for &E" +
        "\n-?  A, (B & C) \u22A2 (C & (B & A))" +
        "\n--Using tactic for &E" +
        "\n--?  B, C, A \u22A2 (C & (B & A))" +
        "\n---Using tactic for &I" +
        "\n---?  B, C, A \u22A2 C  \u25a0" +
        "\n---?  B, C, A \u22A2 (B & A) is the current problem" ==
      ProofToString.derivationToString(proof4))
    assert(
      "Using:-" +
        "\nB" +
        "\nC" +
        "\nA" +
        "\n" +
        "\nDerive:-" +
        "\n(B & A)" ==
      ProofToString.currentProblemToString(proof4))

    val proof5 = proof4.useTactic(AndI, And(B, A)).useTactic(Close, B).useTactic(Close, A).finish
    assert(
      "?  (A & (B & C)) \u22A2 (C & (B & A))" +
        "\n-Using tactic for &E" +
        "\n-?  A, (B & C) \u22A2 (C & (B & A))" +
        "\n--Using tactic for &E" +
        "\n--?  B, C, A \u22A2 (C & (B & A))" +
        "\n---Using tactic for &I" +
        "\n---?  B, C, A \u22A2 C  \u25A0" +
        "\n---?  B, C, A \u22A2 (B & A)" +
        "\n----Using tactic for &I" +
        "\n----?  B, C, A \u22A2 B  \u25A0" +
        "\n----?  B, C, A \u22A2 A  \u25A0" ==
      ProofToString.derivationToString(proof5))
    assert(
      "Finished" ==
      ProofToString.currentProblemToString(proof5))
  }
}
