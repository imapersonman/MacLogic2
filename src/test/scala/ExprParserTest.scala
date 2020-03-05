import org.scalatest.FunSuite

class ExprParserTest extends FunSuite {
  private val A = SL("A")
  private val B = SL("B")
  private val C = SL("C")

  test("ExprParser.parse") {
    assert(ExprParser.parse("A") == Right(A))
    assert(ExprParser.parse("B") == Right(B))
    assert(ExprParser.parse("~A") == Right(Not(A)))
    assert(ExprParser.parse("~~A") == Right(Not(Not(A))))
    assert(ExprParser.parse("~(A)") == Right(Not(A)))
    assert(ExprParser.parse("(~A)") == Right(Not(A)))
    assert(ExprParser.parse("(~(A))") == Right(Not(A)))
    assert(ExprParser.parse("~A -> A") == Right(Imp(Not(A), A)))
    assert(ExprParser.parse("(~A & B) ->(A\\/ B)") == Right(Imp(And(Not(SL("A")), SL("B")), Or(SL("A"), SL("B")))))
    assert(ExprParser.parse("---A") == Right(Not(Not(Not(A)))))
    assert(ExprParser.parse("\\F") == Right(Absurd))
    assert(ExprParser.parse("Absurd") == Right(Absurd))
    assert(ExprParser.parse("--Absurd -> (\\F)") == Right(Imp(Not(Not(Absurd)), Absurd)))
  }

  test("ExprParser.parseList") {
    assert(ExprParser.parseList("") == Right(List()))
    assert(ExprParser.parseList("A") == Right(List(A)))
    assert(ExprParser.parseList("A -> B, C") == Right(List(Imp(A, B), C)))
    assert(ExprParser.parseList("C, A -> B") == Right(List(C, Imp(A, B))))
  }
}
