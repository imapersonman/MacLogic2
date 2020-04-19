import org.scalatest.FunSuite

class ProblemTest extends FunSuite {
  private val A = SL("A")
  private val B = SL("B")

  test("Problem.isClosed") {
    assert(!OpenProblem(SequentExamples.preImpISequent1).isClosed)
    assert(ClosedProblem(Sequent(Seq(SL("A")), SL("A"))).isClosed)
    assert(
      !SplitProblem(Sequent(Seq(A), Not(A)), Close, A, Seq(
        ClosedProblem(Sequent(Seq(A), A)),
        OpenProblem(Sequent(Seq(), A)))).isClosed)
    assert(
      SplitProblem(Sequent(Seq(A), Not(A)), Close, A, Seq(
        ClosedProblem(Sequent(Seq(A), A)),
        ClosedProblem(Sequent(Seq(), A)))).isClosed)
  }

  test("Problem.select") {
    assert(
      ProblemExamples.preAndIProblem1 ==
      ProblemExamples.preAndIProblem1.select(ProblemSelector()))
    assert(
      ClosedProblem(SequentExamples.preAndISequent1) ==
      ClosedProblem(SequentExamples.preAndISequent1).select(ProblemSelector()))
    assert(
      ProblemExamples.postAndIProblem1 ==
      ProblemExamples.postAndIProblem1.select(ProblemSelector()))
    assert(
      OpenProblem(SequentExamples.postAndISequent1_1) ==
      ProblemExamples.postAndIProblem1.select(ProblemSelector(0)))
    assert(
      OpenProblem(SequentExamples.postAndISequent1_2) ==
      ProblemExamples.postAndIProblem1.select(ProblemSelector(1)))
    val selected1 = ProblemExamples.preAndIProblem1
    val deeplyNested = SplitProblem(SequentExamples.preAndISequent1, AndI, And(A, B), Seq(
      SplitProblem(SequentExamples.preAndISequent1, AndI, And(A, B), Seq(
        OpenProblem(SequentExamples.preAndISequent1),
        selected1)),
      OpenProblem(SequentExamples.preAndISequent1)))
    assert(
      selected1 ==
      deeplyNested.select(ProblemSelector(0, 1)))
  }

  test("Problem.replace") {
    assert(
      ProblemExamples.postAndIProblem1 ==
      ProblemExamples.preAndIProblem1.replace(EmptyProblemSelector, ProblemExamples.postAndIProblem1))
    val pre = ProblemExamples.preAndIProblem1
    val preDeeplyNested = SplitProblem(SequentExamples.preAndESequent1, AndE, And(B, A), Seq(
      SplitProblem(SequentExamples.preAndESequent1, AndI, And(A, B), Seq(
        OpenProblem(SequentExamples.preAndESequent1),
        pre)),
      OpenProblem(SequentExamples.preAndESequent1)))
    val post = ProblemExamples.postAndIProblem2
    val postDeeplyNested = SplitProblem(SequentExamples.preAndESequent1, AndE, And(B, A), Seq(
      SplitProblem(SequentExamples.preAndESequent1, AndI, And(A, B), Seq(
        OpenProblem(SequentExamples.preAndESequent1),
        post)),
      OpenProblem(SequentExamples.preAndESequent1)))
    assert(
      postDeeplyNested ==
      preDeeplyNested.replace(ProblemSelector(0, 1), post))
  }

  test("Problem.useTactic Close Success") {
    val sequent1 = Sequent(Seq(A), A)
    assert(ClosedProblem(sequent1) == OpenProblem(sequent1).useTactic(Close, A))
    val sequent2 = Sequent(Seq(A, B), A)
    assert(ClosedProblem(sequent2) == OpenProblem(sequent2).useTactic(Close, A))
    val sequent3 = Sequent(Seq(A, B), B)
    assert(ClosedProblem(sequent3) == OpenProblem(sequent3).useTactic(Close, B))
    val leftSidedSequent = Sequent(Seq(B), A)
    assert(
      Error("Given Expr must be present on both sides of Sequent to Close Problem") ==
      OpenProblem(leftSidedSequent).useTactic(Close, A))
    val rightSidedSequent = Sequent(Seq(A), B)
    assert(
      Error("Given Expr must be present on both sides of Sequent to Close Problem") ==
      OpenProblem(rightSidedSequent).useTactic(Close, A))
    val neitherSidedSequent = Sequent(Seq(B), B)
    assert(
      Error("Given Expr must be present on both sides of Sequent to Close Problem") ==
      OpenProblem(neitherSidedSequent).useTactic(Close, A))
    assert(
      Error("Cannot use tactic on a ClosedProblem") ==
      ClosedProblem(sequent3).useTactic(Close, Not(A)))
    val sequent4 = Sequent(Seq(And(A, B)), And(A, B))
    assert(
      Error("Cannot use tactic on a ClosedProblem") ==
      ClosedProblem(sequent4).useTactic(AndE, And(A, B)))
  }

  test("Problem.useTactic &I") {
    assert(ProblemExamples.postAndIProblem1 == ProblemExamples.preAndIProblem1.useTactic(AndI, And(A, B)))
    assert(ProblemExamples.postAndIProblem2 == ProblemExamples.preAndIProblem2.useTactic(AndI, And(A, B)))
    assert(ProblemExamples.postAndIProblem3 == ProblemExamples.preAndIProblem3.useTactic(AndI, And(A, B)))
    assert(
      ProblemExamples.postAndISplitError ==
      SplitProblem(SequentExamples.preAndISequent1, AndI, And(A, B), Seq(
        OpenProblem(SequentExamples.postAndISequent1_1),
        OpenProblem(SequentExamples.postAndISequent1_2))).useTactic(AndI, And(A, B)))
    assert(ProblemExamples.postAndIWrongExprError == ProblemExamples.preAndIProblem1.useTactic(AndI, And(B, A)))
    assert(
      ProblemExamples.postAndIWrongConnectiveError ==
      OpenProblem(SequentExamples.preOrISequent1).useTactic(AndI, Or(A, B)))
  }

  test("Problem.useTactic &E") {
    assert(ProblemExamples.postAndEProblem1 == ProblemExamples.preAndEProblem1.useTactic(AndE, And(B, A)))
    assert(ProblemExamples.postAndEProblem2 == ProblemExamples.preAndEProblem2.useTactic(AndE, And(B, A)))
    assert(ProblemExamples.postAndEProblem3 == ProblemExamples.preAndEProblem3.useTactic(AndE, And(B, A)))
    assert(ProblemExamples.postAndEWrongExprError == ProblemExamples.preAndEProblem1.useTactic(AndE, And(A, B)))
    assert(
      ProblemExamples.postAndEWrongConnectiveErrorProblem ==
      OpenProblem(SequentExamples.preOrESequent1).useTactic(AndE, Or(B, A)))
  }

  test("Problem.useTactic \\/Il") {
    assert(ProblemExamples.postOrILeftProblem1 == ProblemExamples.preOrIProblem1.useTactic(OrILeft, Or(A, B)))
    assert(ProblemExamples.postOrILeftProblem2 == ProblemExamples.preOrIProblem2.useTactic(OrILeft, Or(A, B)))
    assert(ProblemExamples.postOrILeftProblem3 == ProblemExamples.preOrIProblem3.useTactic(OrILeft, Or(A, B)))
    assert(ProblemExamples.postOrIRightProblem1 == ProblemExamples.preOrIProblem1.useTactic(OrIRight, Or(A, B)))
    assert(ProblemExamples.postOrIRightProblem2 == ProblemExamples.preOrIProblem2.useTactic(OrIRight, Or(A, B)))
    assert(ProblemExamples.postOrIRightProblem3 == ProblemExamples.preOrIProblem3.useTactic(OrIRight, Or(A, B)))
    assert(
      ProblemExamples.postOrILeftWrongConnectiveError ==
      OpenProblem(SequentExamples.preAndISequent1).useTactic(OrILeft, And(A, B)))
    assert(
      ProblemExamples.postOrIRightWrongConnectiveErrorProblem ==
      OpenProblem(SequentExamples.preAndISequent1).useTactic(OrIRight, And(A, B)))
  }

  test("Problem.useTactic \\/E") {
    assert(ProblemExamples.postOrEProblem1 == ProblemExamples.preOrEProblem1.useTactic(OrE, Or(B, A)))
    assert(ProblemExamples.postOrEProblem2 == ProblemExamples.preOrEProblem2.useTactic(OrE, Or(B, A)))
    assert(ProblemExamples.postOrEProblem3 == ProblemExamples.preOrEProblem3.useTactic(OrE, Or(B, A)))
    assert(
      ProblemExamples.postOrEWrongConnectiveError ==
      OpenProblem(SequentExamples.preAndESequent1).useTactic(OrE, And(B, A)))
  }

  test("Problem.useTactic ->I") {
    assert(ProblemExamples.postImpIProblem1 == ProblemExamples.preImpIProblem1.useTactic(ImpI, Imp(A, B)))
    assert(ProblemExamples.postImpIProblem2 == ProblemExamples.preImpIProblem2.useTactic(ImpI, Imp(A, B)))
    assert(ProblemExamples.postImpIProblem3 == ProblemExamples.preImpIProblem3.useTactic(ImpI, Imp(A, B)))
    assert(
      ProblemExamples.postImpIWrongConnectiveError ==
      OpenProblem(SequentExamples.preOrISequent1).useTactic(ImpI, Or(A, B)))
  }

  test("Problem.useTactic ->E") {
    assert(ProblemExamples.postImpEProblem1 == ProblemExamples.preImpEProblem1.useTactic(ImpE, Imp(A, B)))
    assert(ProblemExamples.postImpEProblem2 == ProblemExamples.preImpEProblem2.useTactic(ImpE, Imp(A, B)))
    assert(ProblemExamples.postImpEProblem3 == ProblemExamples.preImpEProblem3.useTactic(ImpE, Imp(A, B)))
    assert(
      ProblemExamples.postImpEWrongConnectiveError ==
      OpenProblem(SequentExamples.preAndESequent1).useTactic(ImpE, And(B, A)))
  }

  test("Problem.useTactic ~I") {
    assert(ProblemExamples.postNotIProblem1 == ProblemExamples.preNotIProblem1.useTactic(NotI, Not(A)))
    assert(ProblemExamples.postNotIProblem2 == ProblemExamples.preNotIProblem2.useTactic(NotI, Not(A)))
    assert(ProblemExamples.postNotIProblem3 == ProblemExamples.preNotIProblem3.useTactic(NotI, Not(A)))
    assert(
      ProblemExamples.postNotIWrongConnectiveError ==
      OpenProblem(SequentExamples.preImpISequent1).useTactic(NotI, Imp(A, B)))
  }

  test("Problem.useTactic ~E") {
    assert(ProblemExamples.postNotEProblem1 == ProblemExamples.preNotEProblem1.useTactic(NotE, Not(A)))
    assert(ProblemExamples.postNotEProblem2 == ProblemExamples.preNotEProblem2.useTactic(NotE, Not(A)))
    assert(ProblemExamples.postNotEProblem3 == ProblemExamples.preNotEProblem3.useTactic(NotE, Not(A)))
    assert(
      ProblemExamples.postNotEWrongConnectiveError ==
      OpenProblem(SequentExamples.preOrESequent1).useTactic(NotE, Or(B, A)))
  }

  test("Problem.useTactic EFQ") {
    val preEFQSequent1 = Sequent(Seq(Absurd), B)
    val preEFQProblem1 = OpenProblem(preEFQSequent1)
    val postEFQProblem1 = SplitProblem(preEFQSequent1, EFQ, B, Seq(OpenProblem(Sequent(Seq(B, Absurd), B))))
    assert(postEFQProblem1 == preEFQProblem1.useTactic(EFQ, B))
  }

  test("Problem.useTactic DN") {
    val preDNSequent1 = Sequent(Seq(), A)
    val preDNSequent2 = Sequent(Seq(B), Not(A))
    val preDNSequent3 = Sequent(Seq(B, And(A, B)), Not(Not(A)))
    val postDNSequent1 = Sequent(Seq(), Not(Not(A)))
    val postDNSequent2 = Sequent(Seq(B), Not(Not(Not(A))))
    val postDNSequent3 = Sequent(Seq(B, And(A, B)), Not(Not(Not(Not(A)))))
    val preDNProblem1: Problem = OpenProblem(preDNSequent1)
    val preDNProblem2: Problem = OpenProblem(preDNSequent2)
    val preDNProblem3: Problem = OpenProblem(preDNSequent3)
    val postDNProblem1: Problem = SplitProblem(preDNSequent1, DN, A, Seq(OpenProblem(postDNSequent1)))
    val postDNProblem2: Problem = SplitProblem(preDNSequent2, DN, Not(A), Seq(OpenProblem(postDNSequent2)))
    val postDNProblem3: Problem = SplitProblem(preDNSequent3, DN, Not(Not(A)), Seq(OpenProblem(postDNSequent3)))
    val postDNProblem3Error: Error = Error("Given Expr must equal rhs to apply DN")
    assert(postDNProblem1 == preDNProblem1.useTactic(DN, A))
    assert(postDNProblem2 == preDNProblem2.useTactic(DN, Not(A)))
    assert(postDNProblem3 == preDNProblem3.useTactic(DN, Not(Not(A))))
    assert(postDNProblem3Error == preDNProblem3.useTactic(DN, Not(A)))
  }
}
