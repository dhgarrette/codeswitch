package dhg.codeswitch

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.util.StringUtil._
import dhg.util.Pattern._
import scalaz._
import Scalaz._

object Evaluation {

  def buildModelFromCorpusFile(fn: String, n: Int = 5, lambda: Double = 1.0): LanguageModel = {
    val data = File(fn).readLines.flatMap(_.splitWhitespace).toVector
    NgramModel.buildInterpolatedNgramModel(data, n, lambda)
  }

  def main(args: Array[String]): Unit = {
    val lms = Vector(
      "english" -> (buildModelFromCorpusFile("lmdata/en.txt"), 0.5),
      "spanish" -> (buildModelFromCorpusFile("lmdata/es.txt"), 0.5))

    //    for ((name, (lm, prior)) <- lms) {
    //      println(name)
    //      val w = "sleep"
    //              println(("0000"+w+"0").sliding(5).map{ ngram =>
    //                val ctx = ngram.dropRight(1)
    //                val c = ngram.takeRight(1)
    //                val p = lm.prob(ctx, c)
    //                println(f"$ctx $c ${p}")
    //                p
    //              }.product)
    //      println(lm.stringProb(w))
    //      println(lm.stringProb(w) * prior)
    //    }

    val cslm = new CodeSwitchWordTrigramModel(lms.map(_._2), 3)
    val eval = new Evaluator("test-data/trialDataEnEswithOffsets.txt", lms.map(_._1))
    val (correct, total) = eval.data.map { sentence =>
      val responses =
        for (((word, guessLangId), gold) <- (sentence.map(_._1) zipSafe cslm.decode(sentence.map(_._1))) zipSafe sentence.map(_._2)) yield {
          val guess = lms.map(_._1).apply(guessLangId)
          println(f"$word : $guess%-10s $gold%-10s  ${if (gold.forall(_ == guess)) "" else "X"}")
          (gold.exists(_ == guess), gold.isDefined)
        }
      val correct = responses.count(_._1)
      val total = responses.count(_._2)
      println(f"Sentence: ${correct / total.toDouble}  ($correct / $total)\n\n")
      (correct, total)
    }.reduce(_ |+| _)

    println(f"Final: ${correct / total.toDouble}  ($correct / $total)")
  }

}

class Evaluator(
  fn: String = "test-data/trialDataEnEswithOffsets.txt",
  languageNames: Vector[String] = Vector("english", "spanish")) {

  val OffsetRe = "(\\d{18}\t\\d+)\t(\\d+)\t(\\d+)\t(.+)".r
  val Re = "(\\d{18}\t\\d+)\t(.*)".r
  val LangId = "lang(\\d)".r
  def data: Iterator[Vector[(String, Option[String])]] = {
    val offsetMap = {
      val m = File(fn.dropRight(3) + "tsv").readLines.map {
        case OffsetRe(id, UInt(start), UInt(end), lang) =>
          id -> (start, end, lang)
      }.groupByKey
      m.mapVals(_.sortBy(_._1))
    }

    File(fn).readLines
      .splitWhere(Re.matches(_), KeepDelimiter.KeepDelimiterAsFirst)
      .map(_.mkString(" ").trim).filter(_.nonEmpty)
      .collect {
        case Re(id, text) if text != "Not Found" =>
          for {
            (start, end, lang) <- offsetMap(id)
            if start < text.length && end < text.length
          } yield {
            val langName = lang match {
              case LangId(UInt(n)) => Some(languageNames(n - 1))
              case _ => None
            }
            (text.substring(start, end + 1).replace("0", "1"), langName)
          }
      }
  }

}