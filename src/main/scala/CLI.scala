import scala.swing._
import scala.swing.event.ButtonClicked

class CLIFrame(input: TextField, output: TextArea, goButton: Button) extends Frame {
  this.title = "MacLogic2 Command-Line"
  this.preferredSize = new Dimension(800, 600)

  private val inputPanel = new BorderPanel {
    this.add(new Label("input"), BorderPanel.Position.West)
    this.add(input, BorderPanel.Position.Center)
    this.add(goButton, BorderPanel.Position.East)
  }

  this.contents = new BorderPanel {
    this.add(new ScrollPane(output), BorderPanel.Position.Center)
    this.add(inputPanel, BorderPanel.Position.South)
    this.border = Swing.EmptyBorder(10, 10, 10, 10)
  }

  this.input.requestFocusInWindow()
  this.output.editable = false
  this.defaultButton = this.goButton
}

class CurrentProblemFrame(textArea: TextArea) extends Frame {
  this.title = "Current Problem"
  this.preferredSize = new Dimension(200, 200)

  this.contents = this.textArea
  this.textArea.border = Swing.EmptyBorder(10, 10, 10, 10)
  this.textArea.editable = false
}

object WindowedController extends Reactor with App {
  // setting up the CLI window.
  val input: TextField = new TextField()
  val output: TextArea = new TextArea { rows = 20; lineWrap = true; wordWrap = true }
  val go: Button = new Button("Go")
  val cli: CLIFrame = new CLIFrame(this.input, this.output, this.go)
  this.cli.visible = true

  // setting up the Current Problem window.
  val currentProblemTextArea = new TextArea { rows = 20; lineWrap = true; wordWrap = true }
  val currentProblemFrame = new CurrentProblemFrame(this.currentProblemTextArea)
  this.currentProblemFrame.visible = true

  this.listenTo(this.go)
  this.reactions += {
    case ButtonClicked(`go`) => this.goPressed()
  }

  private var currentMode: CLIMode = CLIExpectPremises
  this.logToConsole(this.currentMode.output)
  private def goPressed(): Unit = {
    val input = this.input.text
    this.input.text = ""
    this.currentMode = this.handleNextModeOnInput(input)
    this.logToConsole(this.currentMode.output)
  }

  private def handleNextModeOnInput(str: String): CLIMode = this.currentMode.next(str) match {
    case mode: CLIExpectTactic => {
      this.currentProblemTextArea.text = ProofToString.currentProblemToString(mode.proof)
      mode
    }
    case CLIError(message, nextMode) => {
      this.logToConsole(message)
      nextMode
    }
    case other => other
  }

  def logToConsole(str: String): Unit = this.output.text += str + "\n"
}
