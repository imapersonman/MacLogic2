import org.scalatest.FunSuite

class ProofStackTest extends FunSuite {
  private val A = SL("A")
  private val B = SL("B")

  test("ProblemSelectorStack.push") {
    assert(ProblemSelectorStack().push(ProblemSelector(1, 2, 0)) == ProblemSelectorStack(ProblemSelector(1, 2, 0)))
    assert(
      ProblemSelectorStack(ProblemSelector(1, 2, 0)).push(ProblemSelector(0)) ==
      ProblemSelectorStack(ProblemSelector(0), ProblemSelector(1, 2, 0)))
  }

  test("ProblemSelectorStack.append") {
    assert(ProblemSelectorStack().append(ProblemSelectorStack()) == ProblemSelectorStack())
    assert(
      ProblemSelectorStack(ProblemSelector(1), ProblemSelector(2)).append(ProblemSelectorStack()) ==
      ProblemSelectorStack(ProblemSelector(1), ProblemSelector(2)))
    assert(
      ProblemSelectorStack().append(ProblemSelectorStack(ProblemSelector(3), ProblemSelector(2))) ==
      ProblemSelectorStack(ProblemSelector(3), ProblemSelector(2)))
    assert(
      ProblemSelectorStack(ProblemSelector(1, 2)).append(ProblemSelectorStack(ProblemSelector(3))) ==
      ProblemSelectorStack(ProblemSelector(1, 2), ProblemSelector(3)))
  }

  test("Proof.useTactic ongoing") {
    val psStack1 = ProblemSelectorStack(ProblemSelector())
    val sequent0 = Sequent(Seq(And(A, B)), And(B, A))
    val problem1 = OpenProblem(sequent0)
    val proof1 = OngoingProof.start(sequent0)
    assert(proof1 == OngoingProof(problem1, psStack1))

    val psStack2 = ProblemSelectorStack(ProblemSelector(0), ProblemSelector(1))
    val sequent0_0 = Sequent(Seq(And(A, B)), B)
    val sequent0_1 = Sequent(Seq(And(A, B)), A)
    val problem2 = SplitProblem(sequent0, AndI, And(B, A), Seq(
      OpenProblem(sequent0_0),
      OpenProblem(sequent0_1)))
    val proof2 = proof1.useTactic(AndI, And(B, A))
    assert(proof2 == OngoingProof(problem2, psStack2))

    val psStack3 = ProblemSelectorStack(ProblemSelector(0, 0), ProblemSelector(1))
    val sequent0_0_0 = Sequent(Seq(A, B), B)
    val problem3 = SplitProblem(sequent0, AndI, And(B, A), Seq(
      SplitProblem(sequent0_0, AndE, And(A, B), Seq(
        OpenProblem(sequent0_0_0))),
      OpenProblem(sequent0_1)))
    val proof3 = proof2.useTactic(AndE, And(A, B))
    assert(proof3 == OngoingProof(problem3, psStack3))

    val psStack4 = ProblemSelectorStack(ProblemSelector(1))
    val problem4 = SplitProblem(sequent0, AndI, And(B, A), Seq(
      SplitProblem(sequent0_0, AndE, And(A, B), Seq(
        ClosedProblem(sequent0_0_0))),
      OpenProblem(sequent0_1)))
    val proof4 = proof3.useTactic(Close, B)
    assert(proof4 == OngoingProof(problem4, psStack4))

    val psStack5 = ProblemSelectorStack(ProblemSelector(1, 0))
    val sequent0_1_0 = Sequent(Seq(A, B), A)
    val problem5 = SplitProblem(sequent0, AndI, And(B, A), Seq(
      SplitProblem(sequent0_0, AndE, And(A, B), Seq(
        ClosedProblem(sequent0_0_0))),
      SplitProblem(sequent0_1, AndE, And(A, B), Seq(
        OpenProblem(sequent0_1_0)))))
    val proof5 = proof4.useTactic(AndE, And(A, B))
    assert(proof5 == OngoingProof(problem5, psStack5))


    val psStack6 = ProblemSelectorStack()
    val problem6 = SplitProblem(sequent0, AndI, And(B, A), Seq(
      SplitProblem(sequent0_0, AndE, And(A, B), Seq(
        ClosedProblem(sequent0_0_0))),
      SplitProblem(sequent0_1, AndE, And(A, B), Seq(
        ClosedProblem(sequent0_1_0)))))
    val proof6 = proof5.useTactic(Close, A)
    assert(proof6 == OngoingProof(problem6, psStack6))
  }

  test("Proof.useTactic completed") {
    val R = SL("R")
    val S = SL("S")
    val T = SL("T")

    val RtoT = Imp(R, T)
    val StoT = Imp(S, T)
    val RtoStoT = Imp(R, StoT)
    val goal1 = Sequent(Seq(RtoStoT, S), RtoT)
    finishedProofProblemShouldEqual(goal1,
      Seq((ImpI, RtoT), (ImpE, RtoStoT), (Close, R), (ImpE, StoT), (Close, S), (Close, T)),
      ProblemExamples.finalProblem1)

    val A = SL("A")
    val B = SL("B")
    val AorB = Or(A, B)
    val NotBtoNotAorB = Imp(Not(B), Not(AorB))
    val goal2 = Sequent(Seq(Not(A)), NotBtoNotAorB)
    finishedProofProblemShouldEqual(goal2,
      Seq((ImpI, NotBtoNotAorB), (NotI, Not(AorB)), (OrE, AorB), (NotE, Not(A)), (Close, A), (Close, Absurd),
        (NotE, Not(B)), (Close, B), (Close, Absurd)),
      ProblemExamples.finalProblem2)

    val C = SL("C")
    val BorNotC = Or(B, Not(C))
    val AtoBorNotC = Imp(A, BorNotC)
    val NotAtoBorNotC = Imp(Not(A), BorNotC)
    val goal3 = Sequent(Seq(AtoBorNotC, NotAtoBorNotC, Not(B)), Not(C))

    finishedProofProblemShouldEqual(goal3, Seq(
      (NotI, Not(C)), (ImpE, NotAtoBorNotC), (NotI, Not(A)), (ImpE, AtoBorNotC), (Close, A), (OrE, BorNotC),
      (NotE, Not(B)), (Close, B), (Close, Absurd), (NotE, Not(C)), (Close, C), (Close, Absurd), (OrE, BorNotC),
      (NotE, Not(B)), (Close, B), (Close, Absurd), (NotE, Not(C)), (Close, C), (Close, Absurd)),
      ProblemExamples.finalProblem3)
  }

  def finishedProofProblemShouldEqual(goal: Sequent, script: Seq[(Tactic, Prop)], expected: Problem): Unit = {
    var proof: Proof = OngoingProof.start(goal)
    for (line <- script) proof = proof.useTactic(line._1, line._2)
    proof match {
      case OngoingProof(actual, _) => assert(expected == actual)
      // TODO: change to something reasonable
      case _ => throw new IllegalStateException("?????")
    }
  }
}
