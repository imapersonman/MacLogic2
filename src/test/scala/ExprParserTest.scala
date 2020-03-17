import org.scalatest.FunSuite

import scala.util.Success

class ExprParserTest extends FunSuite {
  private val A = SL("A")
  private val B = SL("B")
  private val C = SL("C")

  test("ExprParser.parse") {
    assert(ExprParser.parseExpr("A") == Success(A))
    assert(ExprParser.parseExpr("B") == Success(B))
    assert(ExprParser.parseExpr("~A") == Success(Not(A)))
    assert(ExprParser.parseExpr("~~A") == Success(Not(Not(A))))
    assert(ExprParser.parseExpr("~(A)") == Success(Not(A)))
    assert(ExprParser.parseExpr("(~A)") == Success(Not(A)))
    assert(ExprParser.parseExpr("(~(A))") == Success(Not(A)))
    assert(ExprParser.parseExpr("~A -> A") == Success(Imp(Not(A), A)))
    assert(ExprParser.parseExpr("(~A & B) ->(A\\/ B)") == Success(Imp(And(Not(SL("A")), SL("B")), Or(SL("A"), SL("B")))))
    assert(ExprParser.parseExpr("---A") == Success(Not(Not(Not(A)))))
    assert(ExprParser.parseExpr("\\F") == Success(Absurd))
    assert(ExprParser.parseExpr("Absurd") == Success(Absurd))
    assert(ExprParser.parseExpr("--Absurd -> (\\F)") == Success(Imp(Not(Not(Absurd)), Absurd)))
  }

  test("ExprParser.parseList") {
    assert(ExprParser.parseExprSeq("") == Success(List()))
    assert(ExprParser.parseExprSeq("A") == Success(List(A)))
    assert(ExprParser.parseExprSeq("A -> B, C") == Success(List(Imp(A, B), C)))
    assert(ExprParser.parseExprSeq("C, A -> B") == Success(List(C, Imp(A, B))))
  }
}
