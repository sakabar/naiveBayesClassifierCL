import scala.io.Source
import utils._


object Main{
  def main(args: Array[String]){
    val s = Source.fromFile("./data/train_kakaku.txt")
    try{
      val clf = new Classifier(s.getLines.toArray)
    }
    finally{
      s.close
    }


  }
}
