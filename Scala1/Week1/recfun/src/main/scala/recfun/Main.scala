package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(col: Int, row: Int): Int = {
    if (row == 0 || col == 0 || row == col) {
      1
    } else {
      pascal(col - 1, row - 1) + pascal(col, row - 1);
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    balanceInternal(chars, 0);
  }

  private def balanceInternal(chars: List[Char], openBlockCount: Int): Boolean = {
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

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) {
      0
    } else {
      val remainingMoney = money - coins.head
      if (remainingMoney == 0) {
        countChange(money, coins.tail) + 1
      } else {
        countChange(remainingMoney, coins) + countChange(money, coins.tail)
      }
    }
  }
}
