package utils

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

}

// sealed : 同一ファイル内でのみ継承を許す
// abstract : Cls自体が選択肢になることを防ぐ
// case object : 勝手な継承をさせない
sealed abstract class Cls
case object Pos extends Cls
case object Neg extends Cls



