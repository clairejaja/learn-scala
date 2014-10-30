package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if (r == 0 || r == 1 || c == 0 || c == r) 1
    else pascal(c-1,r-1) + pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def countParentheses(chars: List[Char], x: Int): Boolean =
      // if value is 0, return true
      if (chars.isEmpty) x == 0
      // scan - on every open parentheses, +1, on every close parentheses -1
      else
        if (chars.head == '(') countParentheses(chars.tail,x+1)
        else if (chars.head == ')' && x > 0) countParentheses(chars.tail,x-1)
        else if (chars.head == ')' && x <= 0) false
        else countParentheses(chars.tail,x)

    countParentheses(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =  {
    if (money == 0) 1 // change made!
    else if (money < 0) 0 // change not made
    else if (coins.isEmpty) 0 // no coins to make change
    else {
      countChange(money,coins.tail) + countChange(money-coins.head,coins)
    }
  }
}
