package dhg.codeswitch

import dhg.util._
import scala.Vector
import scala.collection.mutable.{ Map => MMap }

object Test {

  object NgramModel {
    val BoundarySymbol = "0"

    def buildNgramModel(strings: Vector[String], n: Int, lambda: Double, totalNumberOfCharacters: Int) = {
      new SingleNgramModel(getConditionalCounts(strings, n), n, lambda, totalNumberOfCharacters)
    }

    def getNgrams(string: String, n: Int): Iterator[String] = {
      val stringWithStartEnd = (BoundarySymbol * (n - 1)) + string + BoundarySymbol
      stringWithStartEnd.sliding(n)
    }

    def getConditionalCounts(strings: Vector[String], n: Int): Map[String, Map[String, Int]] = {
      val ngrams = strings.flatMap(s => getNgrams(s, n))
      ngrams.groupBy(_.take(n - 1)).map {
        case (ctx, trigramOccurrences) =>
          val lastChars = trigramOccurrences.map(_.drop(n - 1))
          val lastCharCounts = lastChars.groupBy(x => x).map {
            case (lastChar, lastCharOccurences) => lastChar -> lastCharOccurences.size
          }
          ctx -> lastCharCounts
      }
    }
  }

  trait LanguageModel {
    //def n: Int
    def prob(ctx: String, c: String): Double
    def stringProb(ctx: String): Double
  }

  class SingleNgramModel(
    conditionalCounts: Map[String, Map[String, Int]],
    val n: Int,
    lambda: Double,
    totalNumberOfCharacters: Int)
    extends LanguageModel {
    import NgramModel._

    private[this] val contextCountTotals = conditionalCounts.map {
      case (ctx, lastCharCounts) => ctx -> lastCharCounts.map(_._2).sum
    }

    def prob(ctx: String, c: String): Double = {
      assert(ctx.length == n - 1, f"ctx must be length ${n - 1}, was [$ctx]")
      assert(c.length == 1, f"c must be length 1, was [$c]")
      val count = conditionalCounts.getOrElse(ctx, Map.empty).get(c).getOrElse(0)
      val total = contextCountTotals.getOrElse(ctx, 0)
      (count + lambda) / (total + totalNumberOfCharacters * lambda).toDouble
    }

    def stringProb(string: String): Double = {
      val stringWithStartEnd = (BoundarySymbol * (n - 1)) + string + BoundarySymbol
      val ngramProbs =
        for {
          (ctx, counts) <- getConditionalCounts(Vector(stringWithStartEnd), n)
          (lastChar, count) <- counts
        } yield {
          prob(ctx, lastChar) * count
        }
      ngramProbs.product
    }
  }

  class CodeSwitchNgramModel(
    languageModels: Vector[(LanguageModel, Double)],
    wordN: Int) {
    //import NgramModel._

    //for ((lm, prior) <- languageModels) require(lm.n == charN) // all models must have the same `n`

    private[this] val B = -1
    private[this] val E = -2
    private[this] val K = B +: (0 until languageModels.size)

    def decode(words: Vector[String]) = {
      val words2 = words :+ "<E>"
      import collection.mutable.{ Map => MMap }
      var p: Vector[MMap[(Int, Int), Double]] = Vector(MMap((B, B) -> 1.0).withDefaultValue(0.0)) ++ Vector.fill(words.size)(MMap.empty[(Int, Int), Double])
      for (k <- 1 to words.length) {
        println(f"k=$k")
        println(f"p=$p")
        val options =
          for {
            u <- K
            v <- K.drop(1)
          } yield {

            //            for { ((u, v), _) <- p(k-1) } yield {

            print(f"${f"p($k,u=$u,v=$v) = "}%-15s" + f"max { ")
            val options = K.flatMap { w =>
              val x = words2(k-1) // really words(k) ...
              val ps = p(k - 1).getOrElse((w, u), 0.0)
              val qs = q(Vector(w, u), v)
              val es = e(v, x)
              val score = ps * qs * es
              if (score > 0.0) {
                val a = f"p(${k - 1},w=$w,u=$u)=$ps"
                val b = f"q(w=$w,u=$u > v=$v)=$qs"
                val c = f"e(v=$v, x=$x)=$es"
                print(f"$a%-10s * $b%-18s * $c%-15s  = $score    ,    ")
                Some(w -> score)
              }
              else None
            }
            if (options.nonEmpty) {
              val (maxL, maxLP) = options.maxBy(_._2)
              p(k)((u, v)) = maxLP
              println(f" }  = $maxLP")
            }
            else {
              println(f" }  = ")
            }
          }
        println
      }

    }

    def q(ctx: Vector[Int], x: Int) = 0.2
    def e(v: Int, x: String) = {
      val (lm, prior) = languageModels(v)
      lm.stringProb(x) * prior
    }

  }

  def main(args: Array[String]): Unit = {

    //    val lambda = 1.0
    //    val n = 3
    //    // val strings = Vector(
    //    //   "this is a string",
    //    //   "and so is this")
    //    val corpus = Vector(
    //      "tdr.",
    //      "tdw.",
    //      "tmw.",
    //      "amwtd.",
    //      "tcw.",
    //      "tdhtc.")
    //    // val totalNumberOfCharacters = 26
    //
    //    val corpusNgrams = corpus.flatMap(s => NgramModel.getNgrams(s, n))
    //    corpusNgrams.sorted foreach println; println
    //    val corpusConditionalCounts = NgramModel.getConditionalCounts(corpus, n)
    //    corpusConditionalCounts.toVector.sortBy(_._1) foreach println; println
    //
    //    val totalNumberOfCharacters = corpus.flatten.distinct.size + 1
    //    val model = NgramModel.buildNgramModel(corpus, n, lambda, totalNumberOfCharacters)
    //
    //    // println(prob("en", "t"))
    //    // println(prob("is", " "))
    //    // println(prob("is", "0"))
    //    // println(prob("en", "z"))
    //    // println(prob("xy", "z"))
    //
    //    println(model.prob("00", "t"))
    //    println(model.prob("0t", "c"))
    //    println(model.prob("tc", "r"))
    //    println(model.prob("cr", "."))
    //    println(model.prob("r.", "0"))
    //    println(model.stringProb("tcr."))
    //    println("\n\n\n\n")

    val cslm = new CodeSwitchNgramModel(Vector(
      (NgramModel.buildNgramModel("this is a sentence".split("\\s+").toVector, 2, 0.01, 26), 0.5),
      (NgramModel.buildNgramModel("these are a sentences".split("\\s+").toVector, 2, 0.01, 26), 0.5)), 3)
    cslm.decode("thes ise a sentences".split("\\s+").toVector)

  }

}
