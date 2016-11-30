package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Balance")
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c > r || r <= 0) 0
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def direction(char: Char) = if (char == '(') -1 else if (char == ')') 1 else 0

    def check(count: Int, chars: List[Char]): Boolean = {

      def isNextGood(char: Char) = if (count == 0 && direction(char) > 0) false else true

      if (chars.isEmpty && count == 0) true
      else if (chars.nonEmpty && isNextGood(chars.head)) check(count + direction(chars.head), chars.tail)
      else false
    }

    check(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)

  }
}