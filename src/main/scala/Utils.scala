package utils
import scala.math._

class Classifier(val testData: Array[String]) {

 private val kig = Array("、", "。", ",", "-", "「", "」", "．", "[", "]", "%", "％", "&", "＆", "【", "】", "『", "』", "（", "）", "!", "！")
  private val k2 = Array("て", "に", "を", "は", "の", "です", "ます", "が", "ね", "で", "の", "だ")
  private val pro = Array("わたし", "私", "彼", "あなた")
 val stop_word =kig ++ k2  ++ pro


  var count = collection.mutable.Map[(String, Cls), Int]()

  for (line <- testData) {
    val cls = if (line.split(" ")(0) == "+1") Pos else Neg
 
    for (word <- line.split(" ").tail) {
      if (stop_word.contains(word) || (word exists { _.isDigit })
          || (word matches "[a-z]+") || (word matches "[A-Z]+")){ 
        //単語がストップワードだったら何もしない
        //単語が数字を含むなら何もしない
        //単語がアルファベットを含むなら何もしない
      } else {
        if (count.contains(word, cls)) {
          count(word, cls) += 1
        } else {
          count += (word, cls) -> 1
        }
      }
    }
  }
 //未知語
 //本当はStringクラスを継承して未知語を特別に扱いたかったが、
 //Stringクラスを継承することができなかったため失敗
 val unk = "これは未知語です"
 count += (unk, Pos) -> 0
 count += (unk, Neg) -> 0
 
	val vocab = count.keys.map(k => k._1).toList.distinct
  val vocab_num = vocab.size
  val cls_num = Map[Cls, Int](Pos -> count.filterKeys(p => p._2 == Pos).values.sum,
    Neg -> count.filterKeys(p => p._2 == Neg).values.sum)
    
private def hoge(cw:Int, cls:Cls) : Double = log10(cw + 1.0) - log10(cls_num(cls) + vocab_num)
  val probability = count.foldLeft(Map[(String, Cls), Double]()){
   	case (ans, ((w, cls), cw)) => ans + ((w, cls) -> hoge(cw, cls))
   }
 
  def output() {
    for (line <- testData) {
      println(line)
    }
  }
  
  def getProbability(str: String, cls: Cls): Double = {
    if (probability.contains(str, cls)) {
      return probability(str, cls)
    } else {
      return probability(unk, cls)
      // return 1.0 / (cls_num(cls) + vocab_num)
    }
  }

  def classify(words: Array[String]): Cls = {
    //精度下がったorz
    // val d = cls_num(Pos) + cls_num(Neg)
    // val log_Pos_p = log10(cls_num(Pos).toDouble / d)
    // val log_Neg_p = log10(cls_num(Neg).toDouble / d)
     val log_Pos_p = log10(0.5)
     val log_Neg_p = log10(0.5)

    val pos_p = words.map(w => getProbability(w, Pos)).foldLeft(log_Pos_p)(_ + _)
    val neg_p = words.map(w => getProbability(w, Neg)).foldLeft(log_Neg_p)(_ + _)

    return if (pos_p >= neg_p) Pos else Neg

  }
}
// sealed : 同一ファイル内でのみ継承可
// abstract : Cls自体が選択肢の1つになることを防ぐ
// case object : 勝手な継承を防ぐ
sealed abstract class Cls
case object Pos extends Cls
case object Neg extends Cls



