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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def isLrb(char: Char): Int = {
      if (char == '(')
        1
      else
        0
    }
    def isRrb(char: Char): Int = {
      if (char == ')')
        1
      else
        0
    }
    def balanceIn(lrb: Int, rrb: Int, remchars: List[Char]): Boolean = {
      if (remchars.isEmpty)
        lrb == rrb
      else {
        val lrbCurr = lrb + isLrb(remchars.head)
        val rrbCurr = rrb + isRrb(remchars.head)
        (lrbCurr >= rrbCurr) && balanceIn(lrbCurr, rrbCurr, remchars.tail)
      }

    }
    balanceIn(0, 0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(sumLeft:Int, remCoins: List[Int]): Int = {
      if (sumLeft < 0 || remCoins.isEmpty)
        0
      else if (sumLeft == 0)
        1
      else
        count(sumLeft - remCoins.head, remCoins) + count(sumLeft, remCoins.tail)
    }

    if (money == 0 || coins.isEmpty)
      0
    else
      count(money, coins)
  }
}
