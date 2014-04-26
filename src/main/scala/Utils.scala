package utils
import scala.math._


class Classifier(val testData :Array[String]){

  var count = collection.mutable.Map[(String, Cls), Int]()

  for(line <- testData){
    val cls = if(line.split(" ")(0) == "+1") Pos else Neg

    for(word <- line.split(" ").tail){
      if(count.contains(word, cls)){
	count(word, cls) += 1
      }
      else{
	count += (word, cls) -> 1
      }
    }
  }

  val vocab = count.keys.map(k => k._1).toList.distinct.size
  val cls_num = Map[Cls, Int](Pos -> count.filterKeys(p => p._2 == Pos).values.sum,
                              Neg -> count.filterKeys(p => p._2 == Neg).values.sum)


  val probability = count.foldLeft(Map[(String, Cls), Double]())((ans, kv) => ans + (kv._1 -> (kv._2 +1.0) / (cls_num(kv._1._2) + vocab)))

  def output(){
    for(line <- testData){
      println(line)
    }
  }
  def getProbability(str:String, cls:Cls) : Double = {
    if (probability.contains(str, cls)){
      return probability(str, cls)
    }
    else{
      return 1.0 / (cls_num(cls) + vocab) 
    }
  } 

def classify(words: Array[String]) : Cls = {
  val log_Pos_p = log10(0.5)
  val log_Neg_p = log10(0.5)
  //Pos縺ｫ縺ｪ繧狗｢ｺ邇�
  val pos_p = words.map(w => getProbability(w, Pos)).foldLeft(log_Pos_p)(_+_)

  //Neg縺ｫ縺ｪ繧狗｢ｺ邇�
  val neg_p = words.map(w => getProbability(w, Neg)).foldLeft(log_Neg_p)(_+_)
  //val neg_p = words.map().foldL(log_Neg_p)(+)

  return if(pos_p >= neg_p) Pos else Neg

}
}
// sealed : 蜷御ｸ�繝輔ぃ繧､繝ｫ蜀�〒縺ｮ縺ｿ邯呎価繧定ｨｱ縺�
// abstract : Cls閾ｪ菴薙′驕ｸ謚櫁い縺ｫ縺ｪ繧九％縺ｨ繧帝亟縺�
// case object : 蜍晄焔縺ｪ邯呎価繧偵＆縺帙↑縺�
sealed abstract class Cls
case object Pos extends Cls
case object Neg extends Cls



