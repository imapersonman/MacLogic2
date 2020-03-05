import org.scalatest.FunSuite

class SequentTest extends FunSuite {
  private val A = SL("A")
  private val B = SL("B")
  private val C = SL("C")

  test("Sequent.addToLhs") {
    assert(
      Sequent(Seq(C, And(A, B)), And(A, B)) ==
      SequentExamples.preAndISequent1.addToLhs(C, And(A, B)))
    assert(
      Sequent(Seq(Not(B), C), And(A, B)) ==
      SequentExamples.preAndISequent2.addToLhs(Not(B)))
    assert(
      Sequent(Seq(A, B, A, C), C) ==
      Sequent(Seq(B, A, C), C).addToLhs(A))
  }

  test("Sequent.removeFromLhs") {
    assert(
      SequentExamples.preAndISequent1 ==
      SequentExamples.preAndISequent1.removeFromLhs(And(A, B)))
    assert(
      Sequent(Seq(A), And(A, B)) ==
      SequentExamples.preAndISequent3.removeFromLhs(C))
    assert(
      Sequent(Seq(A, C), And(A, B)) ==
      SequentExamples.preAndISequent3.removeFromLhs(B))
  }

  test("Sequent.lhsContains") {
    assert(!SequentExamples.preAndISequent1.lhsContains(And(A, B)))
    assert(SequentExamples.preAndISequent2.lhsContains(C))
    assert(!SequentExamples.preAndISequent3.lhsContains(Not(A)))
  }

  test("Sequent.replaceRhs") {
    assert(
      Sequent(Seq(), And(B, A)) ==
      SequentExamples.preAndISequent1.replaceRhs(And(B, A)))
    assert(
      Sequent(Seq(C), C) ==
      SequentExamples.preAndISequent2.replaceRhs(C))
  }

  test("Sequent.rhsContains") {
    assert(SequentExamples.preAndISequent1.rhsContains(And(A, B)))
    assert(!SequentExamples.preAndISequent2.rhsContains(And(B, A)))
    assert(Sequent(Seq(), C).rhsContains(C))
    assert(!Sequent(Seq(C), B).rhsContains(C))
  }
}
