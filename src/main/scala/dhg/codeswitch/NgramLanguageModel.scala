package dhg.codeswitch

object NgramModel {
  val BoundarySymbol = "0"

  def buildNgramModel(strings: Vector[String], n: Int, lambda: Double): SingleNgramModel = buildNgramModel(strings, n, lambda, strings.flatten.distinct.size)
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

  def buildInterpolatedNgramModel(strings: Vector[String], n: Int, lambda: Double): InterpolatedNgramModel = buildInterpolatedNgramModel(strings, n, lambda, strings.flatten.distinct.size)
  def buildInterpolatedNgramModel(strings: Vector[String], n: Int, lambda: Double, totalNumberOfCharacters: Int) = {
    new InterpolatedNgramModel(geometricDist(n).zipWithIndex.map {
      case (prior, i) => (buildNgramModel(strings, i + 1, lambda: Double, totalNumberOfCharacters: Int), prior)
    }.toVector, n)
  }

  /**
   * Each next element doubles in probability.
   * If n=6, p=0.5:
   *
   *      normalize([0.03125, 0.0625, 0.125, 0.25, 0.5, 1.0])
   *              = [0.016, 0.032, 0.063, 0.127, 0.254, 0.508]
   */
  def geometricDist(n: Int, p: Double = 0.5): IndexedSeq[Double] = {
    val raw = (0 until n).map { k => scala.math.pow(p, n - k) }
    val total = raw.sum
    raw.map(_ / total)
  }
}

trait LanguageModel {
  def prob(ctx: String, c: String): Double
  def stringProb(ctx: String): Double
}

trait NgramModel extends LanguageModel {
  def n: Int
}

abstract class AbstractNgramModel() extends NgramModel {
  import dhg.codeswitch.NgramModel._

  final def stringProb(string: String): Double = {
    val ngramProbs =
      for {
        (ctx, counts) <- getConditionalCounts(Vector(string), n)
        (lastChar, count) <- counts
      } yield {
        prob(ctx, lastChar) * count
      }
    ngramProbs.product
  }

  /**
   * P(c | ctx).
   *
   * If ctx < (n-1), treat it as the beginning of a sequence (prepend "start" tokens).
   * If ctx > (n-1), only use the last (n-1) tokens
   */
  final def prob(ctx: String, c: String): Double = {
    //assert(ctx.length == n - 1, f"ctx must be length ${n - 1}, was [$ctx]")
    assert(c.length == 1, f"c must be length 1, was [$c]")
    val ctxCorrectLength =
      if (ctx.size > n - 1)
        ctx.takeRight(n - 1)
      else if (ctx.size < n - 1)
        (BoundarySymbol * (n - 1)) + ctx
      else
        ctx
    doProb(ctxCorrectLength, c)
  }

  /**
   * P(ctx | c)
   *
   * @param ctx Guaranteed to be length (n-1)
   * @param c   Guaranteed to be length 1
   */
  protected def doProb(ctx: String, c: String): Double

}

class SingleNgramModel(
  conditionalCounts: Map[String, Map[String, Int]],
  val n: Int,
  lambda: Double,
  totalNumberOfCharacters: Int)
  extends AbstractNgramModel() {

  private[this] val contextCountTotals = conditionalCounts.map {
    case (ctx, lastCharCounts) => ctx -> lastCharCounts.map(_._2).sum
  }

  def doProb(ctx: String, c: String): Double = {
    val count = conditionalCounts.getOrElse(ctx, Map.empty).get(c).getOrElse(0)
    val total = contextCountTotals.getOrElse(ctx, 0)
    if (total == 0 && lambda == 0.0) return 0.0
    (count + lambda) / (total + totalNumberOfCharacters * lambda).toDouble
  }

}

class InterpolatedNgramModel(subModels: Vector[(NgramModel, Double)], val n: Int) extends AbstractNgramModel() {
  private[this] val totalPrior = subModels.map(_._2).sum
  def doProb(ctx: String, c: String): Double = {
    subModels.map { case (lm, prior) => lm.prob(ctx.takeRight(lm.n), c) * prior / totalPrior }.sum
  }
}

object NgramModelLanguageModel {

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
  }

}
