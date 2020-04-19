import org.scalatest.FunSuite

import scala.util.{Success, Try}

class UtilsTest extends FunSuite {
  def dumbParser(input: String): Try[Int] = Try(input.toInt)

  test("Utils.parseList") {
    assert(Utils.parseList(dumbParser, "") == Success(Seq()))
    assert(Utils.parseList(dumbParser, "1") == Success(Seq(1)))
    assert(Utils.parseList(dumbParser, "1,2") == Success(Seq(1, 2)))
    assert(Utils.parseList(dumbParser, "1    , 2") == Success(Seq(1, 2)))
    assert(Utils.parseList(dumbParser, "1    , 2,3            ,4, 5") == Success(Seq(1, 2, 3, 4, 5)))
    assert(Utils.parseList(dumbParser, "1  , what? , 2").isFailure)
  }
}
