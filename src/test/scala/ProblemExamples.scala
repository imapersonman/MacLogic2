object ProblemExamples {
  private val A = SL("A")
  private val B = SL("B")

  val preAndIProblem1: Problem = OpenProblem(SequentExamples.preAndISequent1)
  val preAndIProblem2: Problem = OpenProblem(SequentExamples.preAndISequent2)
  val preAndIProblem3: Problem = OpenProblem(SequentExamples.preAndISequent3)
  val postAndIProblem1: Problem = SplitProblem(SequentExamples.preAndISequent1, AndI, And(A, B), Seq(
    OpenProblem(SequentExamples.postAndISequent1_1),
    OpenProblem(SequentExamples.postAndISequent1_2)))
  val postAndIProblem2: Problem = SplitProblem(SequentExamples.preAndISequent2, AndI, And(A, B), Seq(
    OpenProblem(SequentExamples.postAndISequent2_1),
    OpenProblem(SequentExamples.postAndISequent2_2)))
  val postAndIProblem3: Problem = SplitProblem(SequentExamples.preAndISequent3, AndI, And(A, B), Seq(
    OpenProblem(SequentExamples.postAndISequent3_1),
    OpenProblem(SequentExamples.postAndISequent3_2)))
  val postAndISplitError: Error = Error("Cannot use tactic on a SplitProblem")
  val postAndIWrongExprError: Error = Error("Sequent must include given Expr in its rhs to apply I tactic")
  val postAndIWrongConnectiveError: Error = Error("Main connective of &I Expr must be &")

  val preAndEProblem1: Problem = OpenProblem(SequentExamples.preAndESequent1)
  val preAndEProblem2: Problem = OpenProblem(SequentExamples.preAndESequent2)
  val preAndEProblem3: Problem = OpenProblem(SequentExamples.preAndESequent3)
  val postAndEProblem1: Problem = SplitProblem(SequentExamples.preAndESequent1, AndE, And(B, A), Seq(
    OpenProblem(SequentExamples.postAndESequent1)))
  val postAndEProblem2: Problem = SplitProblem(SequentExamples.preAndESequent2, AndE, And(B, A), Seq(
    OpenProblem(SequentExamples.postAndESequent2)))
  val postAndEProblem3: Problem = SplitProblem(SequentExamples.preAndESequent3, AndE, And(B, A), Seq(
    OpenProblem(SequentExamples.postAndESequent3)))
  val postAndEWrongExprError: Error = Error("Sequent must include given Expr in its lhs to apply E tactic")
  val postAndEWrongConnectiveErrorProblem: Error = Error("Main connective of &E Expr must be &")

  val preOrIProblem1: Problem = OpenProblem(SequentExamples.preOrISequent1)
  val preOrIProblem2: Problem = OpenProblem(SequentExamples.preOrISequent2)
  val preOrIProblem3: Problem = OpenProblem(SequentExamples.preOrISequent3)
  val postOrILeftProblem1: Problem = SplitProblem(SequentExamples.preOrISequent1, OrILeft, Or(A, B), Seq(
    OpenProblem(SequentExamples.postOrILeftSequent1)))
  val postOrILeftProblem2: Problem = SplitProblem(SequentExamples.preOrISequent2, OrILeft, Or(A, B), Seq(
    OpenProblem(SequentExamples.postOrILeftSequent2)))
  val postOrILeftProblem3: Problem = SplitProblem(SequentExamples.preOrISequent3, OrILeft, Or(A, B), Seq(
    OpenProblem(SequentExamples.postOrILeftSequent3)))
  val postOrIRightProblem1: Problem = SplitProblem(SequentExamples.preOrISequent1, OrIRight, Or(A, B), Seq(
    OpenProblem(SequentExamples.postOrIRightSequent1)))
  val postOrIRightProblem2: Problem = SplitProblem(SequentExamples.preOrISequent2, OrIRight, Or(A, B), Seq(
    OpenProblem(SequentExamples.postOrIRightSequent2)))
  val postOrIRightProblem3: Problem = SplitProblem(SequentExamples.preOrISequent3, OrIRight, Or(A, B), Seq(
    OpenProblem(SequentExamples.postOrIRightSequent3)))
  val postOrILeftWrongConnectiveError: Error = Error("Main connective of \\/Il Expr must be \\/")
  val postOrIRightWrongConnectiveErrorProblem: Error = Error("Main connective of \\/Ir Expr must be \\/")

  val preOrEProblem1: Problem = OpenProblem(SequentExamples.preOrESequent1)
  val preOrEProblem2: Problem = OpenProblem(SequentExamples.preOrESequent2)
  val preOrEProblem3: Problem = OpenProblem(SequentExamples.preOrESequent3)
  val postOrEProblem1: Problem = SplitProblem(SequentExamples.preOrESequent1, OrE, Or(B, A), Seq(
    OpenProblem(SequentExamples.postOrESequent1_1),
    OpenProblem(SequentExamples.postOrESequent1_2)))
  val postOrEProblem2: Problem = SplitProblem(SequentExamples.preOrESequent2, OrE, Or(B, A), Seq(
    OpenProblem(SequentExamples.postOrESequent2_1),
    OpenProblem(SequentExamples.postOrESequent2_2)))
  val postOrEProblem3: Problem = SplitProblem(SequentExamples.preOrESequent3, OrE, Or(B, A), Seq(
    OpenProblem(SequentExamples.postOrESequent3_1),
    OpenProblem(SequentExamples.postOrESequent3_2)))
  val postOrEWrongConnectiveError: Error = Error("Main connective of \\/E Expr must be \\/")

  val preImpIProblem1: Problem = OpenProblem(SequentExamples.preImpISequent1)
  val preImpIProblem2: Problem = OpenProblem(SequentExamples.preImpISequent2)
  val preImpIProblem3: Problem = OpenProblem(SequentExamples.preImpISequent3)
  val postImpIProblem1: Problem = SplitProblem(SequentExamples.preImpISequent1, ImpI, Imp(A, B), Seq(
    OpenProblem(SequentExamples.postImpISequent1)))
  val postImpIProblem2: Problem = SplitProblem(SequentExamples.preImpISequent2, ImpI, Imp(A, B), Seq(
    OpenProblem(SequentExamples.postImpISequent2)))
  val postImpIProblem3: Problem = SplitProblem(SequentExamples.preImpISequent3, ImpI, Imp(A, B), Seq(
    OpenProblem(SequentExamples.postImpISequent3)))
  val postImpIWrongConnectiveError: Error = Error("Main connective of ->I must be ->")

  val preImpEProblem1: Problem = OpenProblem(SequentExamples.preImpESequent1)
  val preImpEProblem2: Problem = OpenProblem(SequentExamples.preImpESequent2)
  val preImpEProblem3: Problem = OpenProblem(SequentExamples.preImpESequent3)
  val postImpEProblem1: Problem = SplitProblem(SequentExamples.preImpESequent1, ImpE, Imp(A, B), Seq(
    OpenProblem(SequentExamples.postImpESequent1_1),
    OpenProblem(SequentExamples.postImpESequent1_2)))
  val postImpEProblem2: Problem = SplitProblem(SequentExamples.preImpESequent2, ImpE, Imp(A, B), Seq(
    OpenProblem(SequentExamples.postImpESequent2_1),
    OpenProblem(SequentExamples.postImpESequent2_2)))
  val postImpEProblem3: Problem = SplitProblem(SequentExamples.preImpESequent3, ImpE, Imp(A, B), Seq(
    OpenProblem(SequentExamples.postImpESequent3_1),
    OpenProblem(SequentExamples.postImpESequent3_2)))
  val postImpEWrongConnectiveError: Error = Error("Main connective of ->E must be ->")

  val preNotIProblem1: Problem = OpenProblem(SequentExamples.preNotISequent1)
  val preNotIProblem2: Problem = OpenProblem(SequentExamples.preNotISequent2)
  val preNotIProblem3: Problem = OpenProblem(SequentExamples.preNotISequent3)
  val postNotIProblem1: Problem = SplitProblem(SequentExamples.preNotISequent1, NotI, Not(A), Seq(
    OpenProblem(SequentExamples.postNotISequent1)))
  val postNotIProblem2: Problem = SplitProblem(SequentExamples.preNotISequent2, NotI, Not(A), Seq(
    OpenProblem(SequentExamples.postNotISequent2)))
  val postNotIProblem3: Problem = SplitProblem(SequentExamples.preNotISequent3, NotI, Not(A), Seq(
    OpenProblem(SequentExamples.postNotISequent3)))
  val postNotIWrongConnectiveError: Error = Error("Main connective of ~I must be ~")

  val preNotEProblem1: Problem = OpenProblem(SequentExamples.preNotESequent1)
  val preNotEProblem2: Problem = OpenProblem(SequentExamples.preNotESequent2)
  val preNotEProblem3: Problem = OpenProblem(SequentExamples.preNotESequent3)
  val postNotEProblem1: Problem = SplitProblem(SequentExamples.preNotESequent1, NotE, Not(A), Seq(
    OpenProblem(SequentExamples.postNotESequent1_1),
    OpenProblem(SequentExamples.postNotESequent1_2)))
  val postNotEProblem2: Problem = SplitProblem(SequentExamples.preNotESequent2, NotE, Not(A), Seq(
    OpenProblem(SequentExamples.postNotESequent2_1),
    OpenProblem(SequentExamples.postNotESequent2_2)))
  val postNotEProblem3: Problem = SplitProblem(SequentExamples.preNotESequent3, NotE, Not(A), Seq(
    OpenProblem(SequentExamples.postNotESequent3_1),
    OpenProblem(SequentExamples.postNotESequent3_2)))
  val postNotEWrongConnectiveError: Error = Error("Main connective of ~E must be ~")

  // Completed problems
  private val R = SL("R")
  private val S = SL("S")
  private val T = SL("T")
  private val RtoT = Imp(R, T)
  private val StoT = Imp(S, T)
  private val RtoStoT = Imp(R, StoT)
  private val goal1 = Sequent(Seq(RtoStoT, S), RtoT)
  val finalProblem1: Problem =
    SplitProblem(goal1, ImpI, RtoT, Seq(
      SplitProblem(Sequent(Seq(R, RtoStoT, S), T), ImpE, RtoStoT, Seq(
        ClosedProblem(Sequent(Seq(R, RtoStoT, S), R)),
        SplitProblem(Sequent(Seq(StoT, R, S), T), ImpE, StoT, Seq(
          ClosedProblem(Sequent(Seq(StoT, R, S), S)),
          ClosedProblem(Sequent(Seq(T, R, S), T))))))))

  private val AorB = Or(A, B)
  private val NotBtoNotAorB = Imp(Not(B), Not(AorB))
  // ~A |- ~B -> ~(A \/ B)
  private val goal2 = Sequent(Seq(Not(A)), NotBtoNotAorB)
  val finalProblem2: Problem =
    SplitProblem(goal2, ImpI, NotBtoNotAorB, Seq(
      SplitProblem(Sequent(Seq(Not(B), Not(A)), Not(AorB)), NotI, Not(AorB), Seq(
        SplitProblem(Sequent(Seq(AorB, Not(B), Not(A)), Absurd), OrE, AorB, Seq(
          SplitProblem(Sequent(Seq(A, Not(B), Not(A)), Absurd), NotE, Not(A), Seq(
            ClosedProblem(Sequent(Seq(A, Not(B), Not(A)), A)),
            ClosedProblem(Sequent(Seq(Absurd, A, Not(B)), Absurd)))),
          SplitProblem(Sequent(Seq(B, Not(B), Not(A)), Absurd), NotE, Not(B), Seq(
            ClosedProblem(Sequent(Seq(B, Not(B), Not(A)), B)),
            ClosedProblem(Sequent(Seq(Absurd, B, Not(A)), Absurd))))))))))

  private val C = SL("C")
  private val BorNotC = Or(B, Not(C))
  private val AtoBorNotC = Imp(A, BorNotC)
  private val NotAtoBorNotC = Imp(Not(A), BorNotC)
  private val goal3 = Sequent(Seq(AtoBorNotC, NotAtoBorNotC, Not(B)), Not(C))
  val finalProblem3: Problem =
    SplitProblem(goal3, NotI, Not(C), Seq(
      SplitProblem(Sequent(Seq(C, AtoBorNotC, NotAtoBorNotC, Not(B)), Absurd), ImpE, NotAtoBorNotC, Seq(
        SplitProblem(Sequent(Seq(C, AtoBorNotC, NotAtoBorNotC, Not(B)), Not(A)), NotI, Not(A), Seq(
          SplitProblem(Sequent(Seq(A, C, AtoBorNotC, NotAtoBorNotC, Not(B)), Absurd), ImpE, AtoBorNotC, Seq(
            ClosedProblem(Sequent(Seq(A, C, AtoBorNotC, NotAtoBorNotC, Not(B)), A)),
            SplitProblem(Sequent(Seq(BorNotC, A, C, NotAtoBorNotC, Not(B)), Absurd), OrE, BorNotC, Seq(
              SplitProblem(Sequent(Seq(B, A, C, NotAtoBorNotC, Not(B)), Absurd), NotE, Not(B), Seq(
                ClosedProblem(Sequent(Seq(B, A, C, NotAtoBorNotC, Not(B)), B)),
                ClosedProblem(Sequent(Seq(Absurd, B, A, C, NotAtoBorNotC), Absurd)))),
              SplitProblem(Sequent(Seq(Not(C), A, C, NotAtoBorNotC, Not(B)), Absurd), NotE, Not(C), Seq(
                ClosedProblem(Sequent(Seq(Not(C), A, C, NotAtoBorNotC, Not(B)), C)),
                ClosedProblem(Sequent(Seq(Absurd, A, C, NotAtoBorNotC, Not(B)), Absurd)))))))))),
        SplitProblem(Sequent(Seq(BorNotC, C, AtoBorNotC, Not(B)), Absurd), OrE, BorNotC, Seq(
          SplitProblem(Sequent(Seq(B, C, AtoBorNotC, Not(B)), Absurd), NotE, Not(B), Seq(
            ClosedProblem(Sequent(Seq(B, C, AtoBorNotC, Not(B)), B)),
            ClosedProblem(Sequent(Seq(Absurd, B, C, AtoBorNotC), Absurd)))),
          SplitProblem(Sequent(Seq(Not(C), C, AtoBorNotC, Not(B)), Absurd), NotE, Not(C), Seq(
            ClosedProblem(Sequent(Seq(Not(C), C, AtoBorNotC, Not(B)), C)),
            ClosedProblem(Sequent(Seq(Absurd, C, AtoBorNotC, Not(B)), Absurd))))))))))
}

