trait ProofSpecification {
  def currentProblemToString(problem: Problem): String
  def derivationToString(proof: Proof): String
}
