package dhg.codeswitch

import dhg.util._
import scalaz._
import Scalaz._
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

    eval.printEvalData()
    eval.evaluate(cslm)
  }

}

class Evaluator(
  fn: String = "test-data/trialDataEnEswithOffsets.txt",
  languageNames: Vector[String] = Vector("english", "spanish")) {

  val OffsetRe = "(\\d{18}\t\\d+)\t(-?\\d+)\t(-?\\d+)\t(.+)".r
  val Re = "(\\d{18}\t\\d+)\t(.*)".r
  val LangId = "lang(\\d)".r
  lazy val data: Vector[Vector[(String, Option[String], String, Int, Int)]] = {
    val offsetMap = {
      val m = File(fn.dropRight(3) + "tsv").readLines.map {
        case OffsetRe(id, UInt(start), UInt(end), lang) =>
          id -> (start, end, lang)
      }.groupByKey
      m.mapVals(_.sortBy(_._1))
    }
    // println(offsetMap)

    File(fn).readLines
      .toVector
      .splitWhere(Re.matches(_), KeepDelimiterAsFirst)
      .map(_.mkString(" ")).filter(_.nonEmpty)
      .collect {
        case Re(id, text) if text != "Not Found" && text.trim.nonEmpty =>
          val cleanText = text.trim
                            .replace("&", "&amp;")
                            .replace("<", "&lt;")
                            .replace(">", "&gt;")
          offsetMap(id).flatMap { case (start, end, lang) =>
            val langName = lang match {
              case LangId(UInt(n)) => Some(languageNames(n - 1))
              //case other => Some(other)
              case _ => None
            }

            //println(s"r: $id  $start $end ${(cleanText+"###################").drop(start).take(end - start + 1).replace("0", "1")}")
            if (end < cleanText.length) {
              val word = cleanText.drop(start).take(end - start + 1).replace("0", "1")
                                .replace("&lt;", "<")
                                .replace("&gt;", ">")
                                .replace("&amp;", "&")
              Some((word, langName, id, start, end))
            } else {
              None
            }
          }
      }.toVector
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

  def printEvalData() = {
    writeUsing(File("temp/tagged_sentences.txt")) { w =>
      data.foreach { sentence =>
        val tokenStrings = sentence.map { case (word, gold, id, start, end) => 
          val goldLang = gold match {
            case Some("english") => "en"
            case Some("spanish") => "es"
            //case Some(other) => other
            case None => "x"
          }
          val modifiedWord = word
          s"$modifiedWord|$goldLang"
        }
        if (!tokenStrings.exists(s => s.contains(" ")))
        w.writeLine(tokenStrings.map{case s if s.contains(" ") => s"$s****"; case s => s }.mkString(" "))
      }
    }

    println(s"all tags: ${data.flatMap(_.map(_._2.toString)).toVector.distinct.mkString(", ")}")
  }

}
