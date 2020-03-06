import scala.io.StdIn.readLine

object CLIController extends App {
  var goals = List(
    this.newSequent("-(A & -B) \\/ -(-D & -E), -(E \\/ B), C -> (-E -> (-D & A))", "-C"),
    this.newSequent("(A \\/ B) & (C \\/ D)", "(B \\/ C) \\/ (A & D)"),
    this.newSequent("-(A \\/ B)", "(-A & -B)"),
    this.newSequent("(-A & -B)", "-(A \\/ B)"),
    this.newSequent("(-A \\/ -B)", "-(A & B)"),
    this.newSequent("-(A & B)", "(-A \\/ -B)"),
    this.newSequent("A \\/ B", "-(-A & -B)"),
    this.newSequent("-A & -B", "A \\/ B"),
    this.newSequent("-(-A \\/ -B)", "A & B"),
    this.newSequent("A & B", "-(-A \\/ -B)"),
    this.newSequent("A -> B", "--A -> --B"),
    this.newSequent("--A -> --B", "A -> B"),
    this.newSequent("(A \\/ B) -> (A \\/ C)", "A \\/ (B -> C)"),
    this.newSequent("A \\/ (B -> C)", "(A \\/ B) -> (A \\/ C)"),
    this.newSequent("-(A -> B)", "A & -B"),
    this.newSequent("A & -B", "-(A -> B)"),
    this.newSequent("R -> (V -> (S \\/ T)), -(S \\/ T)", "R -> -V"),
    this.newSequent("-(A \\/ (B & C))", "-(A \\/ B) \\/ -(A \\/ C))"))
  var mode: CLIMode = CLIExpectTactic(OngoingProof.start(goals(17)))
  while (!mode.isFinished) {
    println(mode.output)
    val input = readLine(mode.prompt)
    mode = mode.next(input)
  }
  println(mode.output)

  private def newSequent(premisesString: String, conclusionString: String): Sequent = {
    val premises: List[Expr] = ExprParser.parseList(premisesString) match {
      case Left(error) => throw new IllegalArgumentException(error.message)
      case Right(success) => success
    }
    val conclusion: Expr = ExprParser.parse(conclusionString) match {
      case Left(error) => throw new IllegalArgumentException(error.message)
      case Right(success) => success
    }
    Sequent(premises, conclusion)
  }
}

