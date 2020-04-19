import scala.swing._
import scala.swing.event.ButtonClicked

class CLIFrame(input: TextField, output: TextArea, eventHandler: WindowEventHandler) extends Frame {
  this.title = "MacLogic2 Command-Line"
  this.preferredSize = new Dimension(800, 200)

  private val inputPanel = new BoxPanel(Orientation.Horizontal)
  this.addButton("Restart", inputPanel)
  this.addButton("Back", inputPanel)
  this.inputPanel.contents += this.input
  this.defaultButton = this.addButton("Go", inputPanel)

  this.contents = new BorderPanel {
    this.add(new ScrollPane(output), BorderPanel.Position.Center)
    this.add(inputPanel, BorderPanel.Position.South)
    this.border = Swing.EmptyBorder(10, 10, 10, 10)
  }

  this.input.requestFocusInWindow()
  this.output.editable = false

  def addButton(name: String, panel: BoxPanel): Button = {
    val button = new Button(name)
    this.listenTo(button)
    this.reactions += { case ButtonClicked(`button`) => eventHandler.handleEvent(name) }
    panel.contents += button
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

class MacLogicUi(eventHandler: WindowEventHandler) {
  val FONT_SIZE: Int = 24
  val FONT: Font = Font("Arial", Font.Style.Plain, FONT_SIZE)

  val input: TextField = new TextField()
  val output: TextArea = new TextArea { rows = 5; lineWrap = true; wordWrap = true }

  val cli: CLIFrame = new CLIFrame(this.input, this.output, eventHandler)

  val currentProblemTextArea: TextArea = new TextArea { rows = 10; lineWrap = true; wordWrap = true }
  val currentProblemFrame = new CurrentProblemFrame(this.currentProblemTextArea)

  val problemTreeArea: TextArea = new TextArea { rows = 20; lineWrap = true; wordWrap = true }
  val problemTreeFrame = new ProblemTreeFrame(this.problemTreeArea)

  this.input.font = FONT
  this.output.font = FONT
  this.currentProblemTextArea.font = FONT
  this.problemTreeArea.font = FONT

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

