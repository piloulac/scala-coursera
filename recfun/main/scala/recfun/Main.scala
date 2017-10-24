package recfun

/**
  * Created by pierrelouislacorte on 13/09/2017.
  */

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
    if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    *
    */
  def balance(chars: List[Char]): Boolean = {

    /**
      * (chars.isEmpty) returns true if there is the same number of '(' and ')'
      *
      *  to close a parenthesis it must has already been open somewhere, this induce that
      *  number_open_parenthesis > 0.
      *
      *  @param chars List of characters
      *  @param number_open_parenthesis number of open parentheses.
      *                                 Opening of a parenthesis -> +1
      *                                 Closing -> -1
      *
      *  @return True if the number of open parenthesis is 0
      */
    def innerBalance(number_open_parenthesis: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) number_open_parenthesis == 0
      else if (chars.head == '(') innerBalance(number_open_parenthesis + 1, chars.tail)
      else if (chars.head == ')') number_open_parenthesis > 0 && innerBalance(number_open_parenthesis - 1, chars.tail)
      else innerBalance(number_open_parenthesis, chars.tail)
    }
    if (chars.isEmpty) throw new NoSuchElementException()
    else innerBalance(0,chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    require(coins.sortWith(_ < _) == coins, "Coins list must be sorted! From smaller to bigger")
    def processCountChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (money - coins.head == 0) 1
      else if (money - coins.head < 0 ) 0
      else processCountChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
    processCountChange(money, coins)
  }
}
