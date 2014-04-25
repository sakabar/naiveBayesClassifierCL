import org.scalatest.FunSuite
import utils._

class SetSuite extends FunSuite {

  test("count1"){
    val input = Array("+1 This pen is very good")
    val clfr = new Classifier(input)

    val expected = Map[(String, Cls), Int](("This", Pos) -> 1, ("pen", Pos) -> 1, ("is", Pos) -> 1, ("very", Pos) -> 1, ("good", Pos) -> 1)
    val actual = clfr.count
    assert(expected === actual)
  }

  test("count2"){
    val input = Array("+1 This pen is very good", "-1 This pen is bad")
    val clfr = new Classifier(input)

    val expected = Map[(String, Cls), Int](("This", Pos) -> 1, ("pen", Pos) -> 1, ("is", Pos) -> 1, ("very", Pos) -> 1, ("good", Pos) -> 1, ("This", Neg) -> 1, ("pen", Neg) -> 1, ("is", Neg) -> 1, ("bad", Neg) -> 1)
    val actual = clfr.count
    assert(expected === actual)
  }

  test("count3"){
    val input = Array("+1 This pen is very good", "-1 This pen is bad", "-1 Too bad")
    val clfr = new Classifier(input)

    val expected = Map[(String, Cls), Int](("This", Pos) -> 1, ("pen", Pos) -> 1, ("is", Pos) -> 1, ("very", Pos) -> 1, ("good", Pos) -> 1, ("This", Neg) -> 1, ("pen", Neg) -> 1, ("is", Neg) -> 1, ("bad", Neg) -> 2, ("Too", Neg) -> 1)
    val actual = clfr.count
    assert(expected === actual)
  }

  test("vocab"){
    val input = Array("+1 This pen is very good", "-1 This pen is bad", "-1 Too bad")
    val clfr = new Classifier(input)

    val expected = 7
    val actual = clfr.vocab
    assert(expected === actual)
  }


  test("probability1"){
    val input = Array("+1 This pen is very good", "-1 This pen is bad", "-1 Too bad")
    val clfr = new Classifier(input)

    val expected = Map[(String, Cls), Double](
      ("This", Pos) -> (1+1.0)/(5+7),
      ("pen", Pos) -> (1+1.0)/(5+7),
      ("is", Pos) -> (1+1.0)/(5+7),
      ("very", Pos) -> (1+1.0)/(5+7),
      ("good", Pos) -> (1+1.0)/(5+7),
      ("This", Neg) -> (1+1.0)/(6+7),
      ("pen", Neg) -> (1+1.0)/(6+7),
      ("is", Neg) -> (1+1.0)/(6+7),
      ("bad", Neg) -> (2+1.0)/(6+7),
      ("Too", Neg) -> (1+1.0)/(6+7))
    val actual = clfr.probability
    assert(expected === actual)
  }



  // test("testData"){
  //   val input = Array("+1 This pen is very good")
  //   val clfr = new Classifier(input)

  //   val expected = Map[(String, Cls), Double](("This", Pos) -> 0.2, ("pen", Pos) -> 0.2, ("is", Pos) -> 0.2, ("very", Pos) -> 0.2, ("good", Pos)->0.2)
  //   val actual : Map[(String, Cls), Double] = clfr.probability
  //   assert(expected === actual)
  // }


 // +1 発色 も ” 緑 が 綺麗 ” と か ” 赤 が 鮮やか ” とか 特長 的 な 色 使い は あり ませ ん が 、 地味 に 全 色 を 無難 に 出し て くれる の で 変 な 色 に なっ たり は し ませ ん でし た 。


}
