object MacLogic2 extends App {
  WindowedController.start()
}

trait WindowEventHandler {
  def handleEvent(eventString: String): Unit
}

class MacLogicStringSpecification {
  val tacticSpec: TacticsSpecification = DefaultTacticsSpecification
  val exprSpec: ExprSpecification[Prop] = DefaultExprSpecification
  val seqSpec: SequentSpecification = SequentSpecification(exprSpec)
  val proofSpec: ProofSpecification = ProofToString(tacticSpec, exprSpec, seqSpec)
}

object WindowedController extends WindowEventHandler {
  private val spec = new MacLogicStringSpecification()
  private var modeHistory: Seq[CLIMode] = Seq.empty
  private var currentMode: CLIMode = CLIExpectPremises
  private val ui = new MacLogicUi(this)

  def start(): Unit = {
    this.ui.start()
    this.currentMode = this.currentMode.start(this.ui, this.spec)
  }

  def backPressed(): Unit = {
    if (this.modeHistory.isEmpty)
      this.ui.logToConsole("Can't go back any further")
    else {
      this.ui.logToConsole("Backtracking")
      this.setCurrentMode(this.modeHistory.head)
      this.modeHistory = this.modeHistory.tail
    }
  }

  def restartPressed(): Unit = {
    this.ui.clear()
    this.ui.clearConsole()
    this.modeHistory = Seq.empty
    this.currentMode = CLIExpectPremises
  }

  override def handleEvent(eventString: String): Unit = eventString match {
    case "Restart" => restartPressed()
    case "Back" => backPressed()
    case _ =>
      this.setCurrentMode(this.currentMode.handleEvent(this.ui, this.spec, eventString))
      this.modeHistory = this.currentMode +: this.modeHistory
  }

  private def setCurrentMode(mode: CLIMode): Unit = {
    this.currentMode = mode.start(this.ui, this.spec)
  }
}
