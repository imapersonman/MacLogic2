import scala.swing._
import scala.swing.event.{ButtonClicked, Key, KeyPressed}

class CLIFrame(input: TextField, output: TextArea, eventHandler: WindowEventHandler) extends Frame {
  this.title = "MacLogic2 Command-Line"
  this.preferredSize = new Dimension(800, 200)

  private val inputPanel = new BoxPanel(Orientation.Horizontal)
  this.addButtonToInputPanel("Restart", _ => this.eventHandler.restartPressed())
  this.addButtonToInputPanel("Back", _ => this.eventHandler.backPressed())
  this.inputPanel.contents += this.input
  this.defaultButton = this.addButtonToInputPanel("Go", _ => this.eventHandler.goPressed())

  this.contents = new BorderPanel {
    this.add(new ScrollPane(output), BorderPanel.Position.Center)
    this.add(inputPanel, BorderPanel.Position.South)
    this.border = Swing.EmptyBorder(10, 10, 10, 10)

    this.listenTo(this.keys)
    this.reactions += { case KeyPressed(_, Key.Escape, _, _) => eventHandler.backPressed() }
  }

  this.input.requestFocusInWindow()
  this.output.editable = false

  def addButtonToInputPanel(name: String, effect: Unit => Unit): Button = {
    val button = new Button(name)
    this.listenTo(button)
    this.reactions += { case ButtonClicked(`button`) => effect.apply() }
    this.inputPanel.contents += button
    button
  }
}

class CurrentProblemFrame(textArea: TextArea) extends Frame {
  this.title = "Current Problem"
  this.preferredSize = new Dimension(200, 200)

  this.contents = new ScrollPane(this.textArea)
  this.textArea.border = Swing.EmptyBorder(10, 10, 10, 10)
  this.textArea.editable = false
}

class ProblemTreeFrame(textArea: TextArea) extends Frame {
  this.title = "Derivation"
  this.preferredSize = new Dimension(640, 480)

  this.contents = new ScrollPane(this.textArea)
  this.textArea.border = Swing.EmptyBorder(10, 10, 10, 10)
  this.textArea.editable = false
}

trait WindowEventHandler {
  def goPressed(): Unit
  def backPressed(): Unit
  def restartPressed(): Unit
}

class MacLogicUi(eventHandler: WindowEventHandler) {
  val input: TextField = new TextField()
  val output: TextArea = new TextArea { rows = 5; lineWrap = true; wordWrap = true }

  val cli: CLIFrame = new CLIFrame(this.input, this.output, eventHandler)

  val currentProblemTextArea: TextArea = new TextArea { rows = 10; lineWrap = true; wordWrap = true }
  val currentProblemFrame = new CurrentProblemFrame(this.currentProblemTextArea)

  val problemTreeArea: TextArea = new TextArea { rows = 20; lineWrap = true; wordWrap = true }
  val problemTreeFrame = new ProblemTreeFrame(this.problemTreeArea)

  def start(): Unit = {
    this.currentProblemFrame.visible = true
    this.problemTreeFrame.visible = true
    this.cli.visible = true
  }

  def clear(): Unit = {
    this.currentProblemTextArea.text = ""
    this.problemTreeArea.text = ""
  }

  def logToConsole(str: String): Unit = this.output.text += str + "\n"

  def clearConsole(): Unit = this.output.text = ""
}

object MacLogic2 extends App {
  WindowedController.start()
}

object WindowedController extends WindowEventHandler {
  private var modeHistory: Seq[CLIMode] = Seq.empty
  private var currentMode: CLIMode = CLIExpectPremises
  private val ui = new MacLogicUi(this)

  def start(): Unit = {
    this.ui.start()
    this.setCurrentMode(this.currentMode)
  }

  override def goPressed(): Unit = {
    val input = this.ui.input.text
    this.ui.input.text = ""
    this.modeHistory = this.currentMode +: this.modeHistory
    this.setCurrentMode(this.handleNextModeOnInput(input))
  }

  override def backPressed(): Unit = {
    if (this.modeHistory.isEmpty)
      this.ui.logToConsole("Can't go back any further")
    else {
      this.ui.logToConsole("Backtracking")
      this.setCurrentMode(this.modeHistory.head)
      this.modeHistory = this.modeHistory.tail
    }
  }

  override def restartPressed(): Unit = {
    this.ui.clear()
    this.ui.clearConsole()
    this.modeHistory = Seq.empty
    this.setCurrentMode(CLIExpectPremises)
  }

  private def setCurrentMode(mode: CLIMode): Unit = {
    this.currentMode = mode
    this.currentMode.updateUi(this.ui)
  }

  private def handleNextModeOnInput(str: String): CLIMode = this.currentMode.next(str) match {
    case CLIError(message, nextMode) =>
      this.ui.logToConsole(message)
      nextMode
    case other => other
  }
}
