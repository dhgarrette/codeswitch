package dhg.codeswitch

import dhg.util.math.LogDouble
import scala.Vector
import scala.collection.mutable.{ Map => MMap }
import scala.collection.mutable.{ Buffer => MSeq }

class CodeSwitchWordUnigramModel(languageModels: Vector[(LanguageModel, LogDouble)]) {

  def decode(words: Vector[String]): Vector[Int] = {
    words.map { w =>
      languageModels.zipWithIndex.map { case ((lm, prior), i) => i -> (LogDouble(lm.stringProb(w)) * prior) }.maxBy(_._2)._1
    }
  }

}

object CodeSwitchWordUnigramModel {

  def main(args: Array[String]): Unit = {

    val lms = Vector(
      //      "english" -> (NgramModel.buildInterpolatedNgramModel("this is a sentence".split("\\s+").toVector, 2, 0.01, 26), 0.5),
      //      "french" -> (NgramModel.buildInterpolatedNgramModel("these are a sentences".split("\\s+").toVector, 2, 0.01, 26), 0.5))
      "english" -> (NgramModel.buildNgramModel("this is a sentence".split("\\s+").toVector, 3, 0.01, 26), LogDouble(0.5)),
      "french" -> (NgramModel.buildNgramModel("these are a sentences".split("\\s+").toVector, 3, 0.01, 26), LogDouble(0.5)))
    val cslm = new CodeSwitchWordUnigramModel(lms.map(_._2))
    //val input = "thes ise a sentences".split("\\s+").toVector
    val input = "this are sentence sentences these are indeed".split("\\s+").toVector
    val decoded = cslm.decode(input)
    assert(input.size == decoded.size)
    for ((t, l) <- (input zip decoded)) {
      println(f"$t%-10s${lms(l)._1}")
    }

    //    val lms = Vector(
    //      "x" -> (NgramModel.buildNgramModel(Vector("x"), 1, 0.0, 1), LogDouble(0.1)),
    //      "a" -> (NgramModel.buildNgramModel(Vector("a"), 1, 0.0, 1), LogDouble(0.1)),
    //      "b" -> (NgramModel.buildNgramModel(Vector("b"), 1, 0.0, 1), LogDouble(0.1)),
    //      "c" -> (NgramModel.buildNgramModel(Vector("c"), 1, 0.0, 1), LogDouble(0.1)),
    //      "d" -> (NgramModel.buildNgramModel(Vector("d"), 1, 0.0, 1), LogDouble(0.1)),
    //      "e" -> (NgramModel.buildNgramModel(Vector("e"), 1, 0.0, 1), LogDouble(0.1)),
    //      "f" -> (NgramModel.buildNgramModel(Vector("f"), 1, 0.0, 1), LogDouble(0.1)))
    //    val cslm = new CodeSwitchWordUnigramModel(lms.map(_._2))
    //    val input = "a b c d e f".split("\\s+").toVector
    //    val decoded = cslm.decode(input)
    //    assert(input.size == decoded.size)
    //    for ((t, l) <- (input zip decoded)) {
    //      println(f"$t%-10s${lms(l)._1}")
    //    }

  }

}
