package reductions

import org.scalameter._
import common._
import reductions.ParallelCountChange.countChange

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if(money == 0) 1
    else if (money < 0 || coins.isEmpty) 0 // not valid
    else {
      val newMoneyTry = money - coins.head
      val newCoinsTry = coins.tail

      countChange(newMoneyTry, coins) + countChange(money, newCoinsTry)
    }
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {

    val shouldUseSequential = threshold(money, coins)
    if (shouldUseSequential) countChange(money, coins)
    else {

      val newMoneyTry = money - coins.head
      val newCoinsTry = coins.tail

        val (newMoneyResult, newCoinResult) = parallel(
          countChange(newMoneyTry, coins),
          countChange(money, newCoinsTry)
        )

      newMoneyResult + newCoinResult

    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = {
    (theMoney : Int, coins: List[Int]) => {
      theMoney <= ((2.0 / 3.0) * startingMoney)
    }
  }


  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold = {
    (theMoney : Int, coins: List[Int]) => {
      coins.length <= ((2.0 / 3.0) * totalCoins)
    }
  }


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (theMoney : Int, coins: List[Int]) => {
      val moneyStartedWithThreshold = moneyThreshold(startingMoney)
      val coinsStartedWithThreshold = totalCoinsThreshold(allCoins.length)

      moneyStartedWithThreshold(theMoney, coins) && coinsStartedWithThreshold(theMoney, coins)
    }
  }
}
