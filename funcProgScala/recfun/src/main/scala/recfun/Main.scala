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
      if(c==0 | r==0 | c==r) 1 else pascal(c-1,r-1)+pascal(c,r-1)
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
     def _balance(chars: List[Char], acc: Int = 0): Boolean = {
        if (chars == Nil | acc < 0) acc == 0
        else {
          val x :: xs = chars
          (x,xs) match {
            case ('(' | '[' | '{',_) => _balance(xs, acc+1)
            case (')' | ']' | '}',_) => _balance(xs, acc-1)
            case(_, _) => _balance(xs,acc)
          }
        }
      }
      _balance(chars)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins == Nil) 0
      else {
        money match {
          case 0 => 1
          case x if x < 0 => 0
          case _ => countChange(money, coins.tail) + countChange(money - coins.head, coins)
        }
      }
    }
  }
