package dhg.codeswitch

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.util.StringUtil._
import dhg.util.Pattern._
import scalaz._
import Scalaz._
import dhg.util.math.LogDouble
import dhg.util.Subprocess
/*

target/start dhg.codeswitch.Evaluation



CodeSwitchWordUnigramModel
Final: 0.9058641130870954  (99328 / 109650)

-------

CodeSwitchWordNgramModel: charN=5, wordN=3
Final: 0.9547834017327861  (104692 / 109650)


CodeSwitchWordNgramModel: charN=3, wordN=4
Final: 0.9501960784313725  (104189 / 109650)

CodeSwitchWordNgramModel: charN=5, wordN=4
Final: 0.959671682626539  (105228 / 109650)

CodeSwitchWordNgramModel: charN=6, wordN=4
Final: 0.9606475148198814  (105335 / 109650)

CodeSwitchWordNgramModel: charN=6, wordN=5
Final: 0.9610761513907888  (105382 / 109650)

*/
object Evaluation {

  def buildModelFromCorpusFile(fn: String, n: Int = 6, lambda: Double = 1.0): LanguageModel = {
    val data = File(fn).readLines.flatMap(_.splitWhitespace).toVector
    NgramModel.buildInterpolatedNgramModel(data, n, lambda)
  }

  def main(args: Array[String]): Unit = {
    val lms = Vector(
      ("english", (buildModelFromCorpusFile("lmdata/en.txt"), LogDouble(0.5))),
      ("spanish", (buildModelFromCorpusFile("lmdata/es.txt"), LogDouble(0.5))))

    val eval = new Evaluator("test-data/en_es_training_offsets.txt", lms.map(_._1))
    //val eval = new Evaluator("test-data/trialDataEnEswithOffsets.txt", lms.map(_._1))

    val cslm = new CodeSwitchWordNgramModel(lms.map(_._2), 5)
    //val cslm = new CodeSwitchWordUnigramModel(lms.map(_._2))

    eval.evaluate(cslm)
  }

}

class Evaluator(
  fn: String = "test-data/trialDataEnEswithOffsets.txt",
  languageNames: Vector[String] = Vector("english", "spanish")) {

  val OffsetRe = "(\\d{18}\t\\d+)\t(-?\\d+)\t(-?\\d+)\t(.+)".r
  val Re = "(\\d{18}\t\\d+)\t(.*)".r
  val LangId = "lang(\\d)".r
  def data: Iterator[Vector[(String, Option[String], String, Int, Int)]] = {
    val offsetMap = {
      val m = File(fn.dropRight(3) + "tsv").readLines.map {
        case OffsetRe(id, UInt(start), UInt(end), lang) =>
          id -> (start, end, lang)
      }.groupByKey
      m.mapVals(_.sortBy(_._1))
    }

    File(fn).readLines
      .splitWhere(Re.matches(_), KeepDelimiter.KeepDelimiterAsFirst)
      .map(_.mkString(" ")).filter(_.nonEmpty)
      .collect {
        case Re(id, text) if text != "Not Found" =>
          for {
            (start, end, lang) <- offsetMap(id)
            if start >= 0 && start < text.length && end >= 0 && end < text.length
          } yield {
            val langName = lang match {
              case LangId(UInt(n)) => Some(languageNames(n - 1))
              case _ => None
            }
            (text.substring(start, end + 1).replace("0", "1"), langName, id, start, end)
          }
      }
  }

  def evaluateSimple(cslm: CodeSwitchWordNgramModel) = {
    var correct = 0
    var total = 0
    data.foreach { sentence =>
      val responses =
        for (((word, gold, _, _, _), guess) <- (sentence zipSafe cslm.decode(sentence.map(_._1)).map(languageNames))) yield {
          //println(f"$word : $guess%-10s $gold%-10s  ${if (gold.forall(_ == guess)) "" else "X"}")
          (gold.exists(_ == guess), gold.isDefined)
        }
      val c = responses.count(_._1)
      val t = responses.count(_._2)
      println(f"${f"Sentence: ${c / t.toDouble}%.4f  ($c / $t)"}%-50s ${f"Total: ${correct / total.toDouble}%.4f  ($correct / $total)"}")
      correct += c
      total += t
    }

    println(f"Final: ${correct / total.toDouble}  ($correct / $total)")
  }

  def evaluate(cslm: CodeSwitchWordNgramModel) = {
    writeUsing(File("temp/output.txt")) { w =>
      data.foreach { sentence =>
        for (((word, _, id, start, end), guess) <- (sentence zipSafe cslm.decode(sentence.map(_._1)))) yield {
          //println(f"$word : ${languageNames(guess)}%-10s $gold%-10s  ${if (gold.forall(_ == languageNames(guess))) "" else "X"}")
          w.writeLine(f"$id\t$start\t$end\tlang${guess + 1}")
        }
      }
    }
    Subprocess("evaluation-script/Scripts/evaluateOffsets.pl").args(fn.dropRight(3) + "tsv", "temp/output.txt", "temp/eval.txt").call()
    File("temp/eval.txt").readLines.foreach(println)
  }

}
