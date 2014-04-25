import org.scalatest.FunSuite
import utils._

class SetSuite extends FunSuite {

  test("inc"){
    val input = 1
    val expected : Int = 2
    val actual : Int = Utils.inc(input)
    assert(expected === actual)
  }



}
