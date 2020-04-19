trait CoastlineException extends RuntimeException

class TacticParserException(unrecognizedString: String) extends CoastlineException {
  override def getMessage: String = "Unrecognized tactic: '" + this.unrecognizedString + "'"
}

class ExprParserException(unrecognizedString: String) extends CoastlineException {
  override def getMessage: String = "Unrecognized expression: '" + this.unrecognizedString + "'"
}

class MacLogicException(message: String) extends CoastlineException {
  override def getMessage: String = message
}
