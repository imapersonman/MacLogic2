import scala.swing._
import scala.swing.event.{ButtonClicked, Key, KeyPressed, KeyReleased}

class CLIFrame(input: TextField, output: TextArea, eventHandler: WindowEventHandler) extends Frame {
  this.title = "MacLogic2 Command-Line"
  this.preferredSize = new Dimension(800, 200)

  private val go: Button = new Button("Go")
  private val back: Button = new Button("Back")
  this.listenTo(this.go)
  this.listenTo(this.back)
  this.reactions += {
    case ButtonClicked(`go`) => this.eventHandler.goPressed()
    case ButtonClicked(`back`) => this.eventHandler.backPressed()
  }

  private val inputPanel = new BoxPanel(Orientation.Horizontal) {
    this.contents += back
    this.contents += new Label("input")
    this.contents += input
    this.contents += go
  }

  this.contents = new BorderPanel {
    this.add(new ScrollPane(output), BorderPanel.Position.Center)
    this.add(inputPanel, BorderPanel.Position.South)
    this.border = Swing.EmptyBorder(10, 10, 10, 10)

    this.listenTo(this.keys)
    this.reactions += {
      case KeyReleased(_, Key.Escape, _, _) => eventHandler.backPressed()
    }
  }

  this.input.requestFocusInWindow()
  this.output.editable = false
  this.defaultButton = this.go
}

class CurrentProblemFrame(textArea: TextArea) extends Frame {
  this.title = "Current Problem"
  this.preferredSize = new Dimension(200, 200)

  this.contents = this.textArea
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
}

object MacLogic2 extends App {
  WindowedController.start()
}

object WindowedController extends WindowEventHandler {
  private var modeHistory: Seq[CLIMode] = Seq.empty
  private var currentMode: CLIMode = CLIExpectPremises
  private val ui = new MacLogicUi(this)

  override def goPressed(): Unit = {
    val input = this.ui.input.text
    this.ui.input.text = ""
    this.modeHistory = this.currentMode +: this.modeHistory
    this.currentMode = this.handleNextModeOnInput(input)
    this.logToConsole(this.currentMode.output)
    this.currentMode.updateUi(this.ui)
  }

  def start(): Unit = {
    this.ui.start()
    this.logToConsole(this.currentMode.output)
  }

  override def backPressed(): Unit = {
    if (this.modeHistory.isEmpty)
      this.logToConsole("Can't go back any further")
    else {
      this.logToConsole("Backtracking")
      this.currentMode = this.modeHistory.head
      this.logToConsole(this.currentMode.output)
      this.currentMode.updateUi(this.ui)
      this.modeHistory = this.modeHistory.tail
    }
  }

  private def handleNextModeOnInput(str: String): CLIMode = this.currentMode.next(str) match {
    case CLIError(message, nextMode) => {
      this.logToConsole(message)
      nextMode
    }
    case other => other
  }

  def logToConsole(str: String): Unit = this.ui.output.text += str + "\n"
}
