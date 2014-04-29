import org.scalatest.FunSuite
import utils._
import scala.math._

class SetSuite extends FunSuite {

  test("count1") {
    val input = Array("+1 あ い い")
    val clfr = new Classifier(input)

    val expected = Map[(String, Cls), Int](("あ", Pos) -> 1, ("い", Pos) -> 2)
    val actual = clfr.count
    assert(expected === actual)
  }

  test("count2") {
    val input = Array("+1 あ い", "-1 い う")
    val clfr = new Classifier(input)

    val expected = Map[(String, Cls), Int](("あ", Pos) -> 1,
      ("い", Pos) -> 1,
      ("い", Neg) -> 1,
      ("う", Neg) -> 1)
    val actual = clfr.count
    assert(expected === actual)
  }

  //  test("count3"){
  //    val input = Array("+1 This pen is very good", "-1 This pen is bad", "-1 Too bad")
  //    val clfr = new Classifier(input)
  //
  //    val expected = Map[(String, Cls), Int](("This", Pos) -> 1, ("pen", Pos) -> 1, ("is", Pos) -> 1, ("very", Pos) -> 1, ("good", Pos) -> 1, ("This", Neg) -> 1, ("pen", Neg) -> 1, ("is", Neg) -> 1, ("bad", Neg) -> 2, ("Too", Neg) -> 1)
  //    val actual = clfr.count
  //    assert(expected === actual)
  //  }

  test("vocab") {
    val input = Array("+1 あ い", "-1 い う")
    val clfr = new Classifier(input)

    val expected = 3 + 1 //未知語を含む 
    val actual = clfr.vocab_num
    assert(expected === actual)
  }

  //  test("probability1"){
  //    val input = Array("+1 This pen is very good", "-1 This pen is bad", "-1 Too bad")
  //    val clfr = new Classifier(input)
  //
  //    val expected = Map[(String, Cls), Double](
  //      ("This", Pos) -> (1+1.0)/(5+7),
  //      ("pen", Pos) -> (1+1.0)/(5+7),
  //      ("is", Pos) -> (1+1.0)/(5+7),
  //      ("very", Pos) -> (1+1.0)/(5+7),
  //      ("good", Pos) -> (1+1.0)/(5+7),
  //      ("This", Neg) -> (1+1.0)/(6+7),
  //      ("pen", Neg) -> (1+1.0)/(6+7),
  //      ("is", Neg) -> (1+1.0)/(6+7),
  //      ("bad", Neg) -> (2+1.0)/(6+7),
  //      ("Too", Neg) -> (1+1.0)/(6+7))
  //    val actual = clfr.probability
  //    assert(expected === actual)
  //  }

  test("classify1") {
    val train = Array("+1 これ いいね", "-1 これ よくないね")
    val clfr = new Classifier(train)

    val input = Array("これ", "いいね")
    val actual = clfr.classify(input)
    val expected = Pos
    assert(expected === actual)
  }

  test("sum of probability_pos") {
    val input = Array("+1 あ い", "-1 い う")
    val clfr = new Classifier(input)
    val sum_pos_p = clfr.probability.filterKeys(p => p._2 == Pos).values.map(loged => pow(10.0, loged)).sum
    assert(1.0 === sum_pos_p)

  }

  test("sum of probability_neg") {
    val input = Array("+1 あ い", "-1 い う")
    val clfr = new Classifier(input)
    val sum_neg_p = clfr.probability.filterKeys(p => p._2 == Neg).values.map(loged => pow(10.0, loged)).sum
    assert(1.0 === sum_neg_p)
  }

}
