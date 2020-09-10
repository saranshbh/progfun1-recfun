package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    val opening_bracket = chars.count(_ == '(')
    val closing_bracket = chars.count(_ == ')')

    def balancing(chain: List[Char], bracket:Int) : Boolean = {
      if (chain.isEmpty)
        true
      else if (chain.head == '(') {
        val chain_tail =chain.tail
        bracket > 0 && chain_tail.count(_ == ')') > 0 &&
          balancing(chain_tail, bracket - 1)
      }
      else
        balancing(chain.tail, bracket)
    }


    if (opening_bracket!=  closing_bracket)
      false
    else
      balancing(chars, closing_bracket)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    val coin = coins.sorted
    if ((money < 0) || coin.isEmpty) 0
    else if (money == 1 || money == 0) 1
    else countChange(money - coin.head, coin) + countChange(money, coin.tail)

  }
}
