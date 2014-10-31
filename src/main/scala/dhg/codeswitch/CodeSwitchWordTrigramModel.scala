package dhg.codeswitch

import dhg.util._
import scala.Vector
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Buffer => MSeq }

class CodeSwitchWordTrigramModel(
  languageModels: Vector[(LanguageModel, Double)],
  wordN: Int,
  verbose: Boolean = true) {
  import NgramModel._

  //for ((lm, prior) <- languageModels) require(lm.n == charN) // all models must have the same `n`

  private[this] val B = -1
  private[this] val E = -2
  private[this] val K = B +: (0 until languageModels.size)

  def decode(words: Vector[String]): Vector[Int] = {
    val words2 = words :+ "<E>"
    import collection.mutable.{ Map => MMap }
    val p: Vector[MMap[(Int, Int), Double]] = Vector(MMap((B, B) -> 1.0).withDefaultValue(0.0)) ++ Vector.fill(words.size)(MMap.empty[(Int, Int), Double])
    val bp: Vector[MMap[(Int, Int), Int]] = Vector.fill(words.size + 1)(MMap.empty[(Int, Int), Int])
    for (k <- 1 to words.length) {
      if (verbose) println(f"k=$k")
      if (verbose) println(f"p=$p")
      if (verbose) println(f"bp=$bp")
      val options =
        for {
          u <- K
          v <- K.drop(1)
        } yield {

          //            for { ((u, v), _) <- p(k-1) } yield {

          if (verbose) print(f"${f"p($k,u=$u,v=$v) = "}%-15s" + f"max { ")
          val options = K.flatMap { w =>
            val x = words2(k - 1) // really words(k) ...
            val ps = p(k - 1).getOrElse((w, u), 0.0)
            val qs = t(Vector(w, u), v)
            val es = e(v, x)
            val score = ps * qs * es
            if (score > 0.0) {
              if (verbose) print(f"${f"p(${k - 1},w=$w,u=$u)=$ps"}%-10s * ${f"t(w=$w,u=$u > v=$v)=$qs"}%-18s * ${f"e(v=$v, x=$x)=$es"}%-15s  = $score    ,    ")
              Some(w -> score)
            }
            else None
          }
          if (options.nonEmpty) {
            val (maxL, maxLP) = options.maxBy(_._2)
            p(k)((u, v)) = maxLP
            bp(k)((u, v)) = maxL
            if (verbose) println(f" }  = $maxLP")
          }
          else {
            if (verbose) println(f" }  = ")
          }
        }
      println
    }

    if (verbose) println("\nBackwards:")
    if (verbose) println(f"bp=$bp")
    val y = MSeq.fill(words.size - (wordN - 1))(-1) ++ {
      (for {
        u <- K.drop(1)
        v <- K.drop(1)
      } yield {
        Vector(u, v) -> p(words.size).getOrElse((u, v), 0.0) // * t(Vector(w, u), E)
      }).maxBy(_._2)._1
    }
    for (k <- (0 to (words.size - (wordN - 0))).reverse) {
      if (verbose) println(f"k=$k")
      if (verbose) println(f"y=$y")

      y(k) = bp(k + 3)(y(k + 1), y(k + 2))
      if (verbose) println(f"decode y($k)=${y(k)}")
    }
    if (verbose) println(f"y=$y")
    assert(y.size == words.size)
    y.toVector
  }

  private[this] val trHistoryLengthWeight = geometricDist(wordN + 1, 0.5)

  /**
   * Probability of transitioning to language `x`, given language context `ctx`.
   * Probability is calculated based on the number of `x`s in a row leading
   * up to this token.
   */
  def t(ctx: Vector[Int], x: Int): Double = {
    val history = ctx.reverse.takeWhile(_ == x).size
    trHistoryLengthWeight(history)
  }

  def e(v: Int, x: String) = {
    val (lm, prior) = languageModels(v)
    lm.stringProb(x) * prior
  }

}

object CodeSwitchWordTrigramModel {

  def main(args: Array[String]): Unit = {

    val lms = Vector(
      //      "english" -> (NgramModel.buildInterpolatedNgramModel("this is a sentence".split("\\s+").toVector, 2, 0.01, 26), 0.5),
      //      "french" -> (NgramModel.buildInterpolatedNgramModel("these are a sentences".split("\\s+").toVector, 2, 0.01, 26), 0.5))
      "english" -> (NgramModel.buildNgramModel("this is a sentence".split("\\s+").toVector, 3, 0.01, 26), 0.5),
      "french" -> (NgramModel.buildNgramModel("these are a sentences".split("\\s+").toVector, 3, 0.01, 26), 0.5))
    val cslm = new CodeSwitchWordTrigramModel(lms.map(_._2), 3)
    //val input = "thes ise a sentences".split("\\s+").toVector
    val input = "this are sentence sentences these are indeed".split("\\s+").toVector
    val decoded = cslm.decode(input)
    assert(input.size == decoded.size)
    for ((t, l) <- (input zip decoded)) {
      println(f"$t%-10s${lms(l)._1}")
    }

    //    val lms = Vector(
    //      "x" -> (NgramModel.buildNgramModel(Vector("x"), 1, 0.0, 1), 0.1),
    //      "a" -> (NgramModel.buildNgramModel(Vector("a"), 1, 0.0, 1), 0.1),
    //      "b" -> (NgramModel.buildNgramModel(Vector("b"), 1, 0.0, 1), 0.1),
    //      "c" -> (NgramModel.buildNgramModel(Vector("c"), 1, 0.0, 1), 0.1),
    //      "d" -> (NgramModel.buildNgramModel(Vector("d"), 1, 0.0, 1), 0.1),
    //      "e" -> (NgramModel.buildNgramModel(Vector("e"), 1, 0.0, 1), 0.1),
    //      "f" -> (NgramModel.buildNgramModel(Vector("f"), 1, 0.0, 1), 0.1))
    //    val cslm = new CodeSwitchWordTrigramModel(lms.map(_._2), 3)
    //    val input = "a b c d e f".split("\\s+").toVector
    //    val decoded = cslm.decode(input)
    //    assert(input.size == decoded.size)
    //    for ((t, l) <- (input zip decoded)) {
    //      println(f"$t%-10s${lms(l)._1}")
    //    }

  }

}