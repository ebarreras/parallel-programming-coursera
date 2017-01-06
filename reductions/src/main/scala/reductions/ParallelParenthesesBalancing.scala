package reductions

import scala.annotation._
import org.scalameter._
import common._

import scala.util.Try

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @annotation.tailrec
    def go(open: Int, pos: Int, until: Int): Int =
      if (pos >= until) open
      else (open, chars.charAt(pos)) match {
        case (_, '(') => go(open + 1, pos + 1, until)
        case (o, ')') if o > 0 => go(open - 1, pos + 1, until)
        case (_, ')') => throw new IllegalStateException()
        case (_, _) => go(open, pos + 1, until)
      }

    Try(go(0, 0, chars.length)).map(_ == 0).getOrElse(false)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, closed: Int, open: Int): (Int,Int) = {
      @annotation.tailrec
      def go(counts: (Int, Int), pos: Int): (Int, Int) =
        if (pos >= until) counts
        else {
          val newCounts = chars.charAt(pos) match {
            case '(' => (counts._1, counts._2 + 1)
            case ')' if counts._2 > 0 => (counts._1, counts._2 - 1)
            case ')' => (counts._1 + 1, counts._2)
            case _ => counts
          }
          go(newCounts, pos + 1)
        }
      go((closed, open), idx)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val m = from + (until - from)/2
        val (c1, c2) = parallel(reduce(from, m), reduce(m, until))
        val bal = c1._2 - c2._1
        if (bal < 0)
          (c1._1 + (-bal), c2._2)
        else
          (c1._1, c2._2 + bal)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
