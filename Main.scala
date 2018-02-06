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
    c match {
      case 0 => 1
      case x if x == `r` => 1
      case y if 0 < y && y < `r` => pascal(y - 1, `r` - 1) + pascal(y, `r` - 1)
      case _ => 0
    }
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def searchForEnd(chars: List[Char]): (Boolean, List[Char]) = {
      chars match {
        case Nil => (false, List.empty)

        case h :: t if h == '(' => {
          val resSearch = searchForEnd(t)
          if (resSearch._1)
            searchForEnd(resSearch._2)
          else
            resSearch
        }
        case h :: t if h == ')' => (true, t)
        case h :: t if h != ')' => searchForEnd(t)
        case _ => (false, List.empty)
      }
    }

    chars match {
      case Nil => true
      case x if x.head == ')' => false
      case h :: t if h == '(' => {
        val resSearch = searchForEnd(t)
        if (resSearch._1)
          balance(resSearch._2)
        else
          false
      }
      case h :: t if h != '(' => balance(t)
      case _ => false
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def calculateTree(coinsToPlay : List[Int], acc : Int) : Int = {
      if(acc > money)
        0
      else if (acc == money)
        1
      else{
        coinsToPlay match {
          case Nil => 0
          case h :: t => {
            calculateTree(coinsToPlay,acc + h) + calculateTree(t, acc)
          }
        }
      }
    }
    calculateTree(coins, 0)

  }
}
