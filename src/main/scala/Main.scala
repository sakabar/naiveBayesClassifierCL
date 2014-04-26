import scala.io.Source
import utils._

object Main {
  def main(args: Array[String]) {
    val trSet = Source.fromFile("./data/train_kakaku.txt")
    val teSet = Source.stdin

    try {
      val clf = new Classifier(trSet.getLines.toArray)

      var ok = 0
      var ng = 0

      for (line <- teSet.getLines) {
        val expected = if (line.split(" ")(0) == "+1") Pos else Neg
        val words = line.split(" ").tail

        if (clf.classify(words) == expected) {
          ok += 1
        } else {
          ng += 1
          for (w <- words) {
            print(w + " ")
          }
          println()
          println(expected + " is Expected")
        }

      }
      println(ok + "/" + (ok + ng) + " : " + (ok.toDouble / (ok + ng)))

    } finally {
      trSet.close
      teSet.close
    }

  }
}
