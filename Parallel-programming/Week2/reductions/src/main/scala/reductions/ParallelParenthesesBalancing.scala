package reductions

import scala.annotation._
import org.scalameter._
import common._
import reductions.ParallelParenthesesBalancing.ParenthesesStatus
import reductions.ParallelParenthesesBalancing.ParenthesesStatus.ParenthesesStatus

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

      def balanceInternal(chars: Array[Char], openBlockCount: Int): Boolean = {
      if (chars.isEmpty) {


        openBlockCount == 0

      } else {
        val newOpenBlockCount = chars.head match {
          case '(' => openBlockCount + 1
          case ')' => openBlockCount - 1
          case _ => openBlockCount
        }


        (newOpenBlockCount >= 0) && balanceInternal(chars.tail, newOpenBlockCount)

      }
    }

    balanceInternal(chars, 0);
  }

  object ParenthesesStatus extends Enumeration {
    type ParenthesesStatus = Value
    val STARTED_WITH_OPENING, STARTED_WITH_CLOSING, NO_PARENTHESES_FOUND = Value
  }


  import ParenthesesStatus._

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, parenthesesBalance: Int, status: ParenthesesStatus): (Int, ParenthesesStatus) = {
      if (idx >= until) {
        // recursion ends here
        (parenthesesBalance, status)
      } else {
        val (newParenthesesBalance, newStatus) = chars(idx) match {
          case '(' => {
            (parenthesesBalance + 1, if (status == NO_PARENTHESES_FOUND) STARTED_WITH_OPENING else status)
          }
          case ')' => {
            (parenthesesBalance - 1, if (status == NO_PARENTHESES_FOUND) STARTED_WITH_CLOSING else status)
          }
          case _ => (parenthesesBalance, status)
        }

        // recursive method call at the end of the method, so that is @tailrec eligible
        traverse(idx + 1, until, newParenthesesBalance, newStatus)
      }
    }

    def reduce(from: Int, until: Int): (Int, ParenthesesStatus) = {

      val length = until - from
      if (length  <= threshold) {
        traverse(from, until, 0, NO_PARENTHESES_FOUND)
      } else {
        val middleIndex = from + length / 2
        val ((leftParenthesesBalance, leftStatus), (rightParenthesesBalance, rightStatus)) = parallel(
          reduce(from, middleIndex),
          reduce(middleIndex, until)
        )

        // combining the balance is simply adding the two together
        val combinedParenthesesBalance = leftParenthesesBalance + rightParenthesesBalance

        // combining the status of the two halves:
        // if left part contained ANY parentheses, take its status, otherwise fall back to right
        val combinedStatus = if (leftStatus != NO_PARENTHESES_FOUND) leftStatus else rightStatus


        (combinedParenthesesBalance, combinedStatus)
      }
    }

    if (chars.length < threshold) {
      // call sequential implementation, if below threshold
      balance(chars)
    } else {
      val (parenthesesBalance, status) = reduce(0, chars.length)
      parenthesesBalance == 0 && (status == STARTED_WITH_OPENING || status == NO_PARENTHESES_FOUND)
    }


  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
