package recfun

object Main {       //> Welcome to the Scala worksheet

  def or(x: Boolean, y: Boolean): Boolean = if(!x) y else true
  //> or: (x: Boolean, y: Boolean)Boolean
  def and(x: Boolean, y: => Boolean): Boolean = if(x) y else false
  //> and: (x: Boolean, y: => Boolean)Boolean

  def pascal(c: Int, r: Int): Int = {
    if (or(c == 0, c == r)) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }                                              //> pascal: (c: Int, r: Int)Int

  pascal(1,1)                                   //> hello
  //| res0: Int = 1

  def balance(chars: List[Char]): Boolean = {
    def bal(chars1: List[Char], count: Int): Boolean = {
      if(chars1.isEmpty)
        if(count == 0) true else false

      else if(chars1.head == '(') bal(chars1.tail,count+1)
      else if(chars1.head == ')') {
        if(count <= 0) false
        else bal(chars1.tail,count-1)
      }
      else bal(chars1.tail,count)
    }
    bal(chars,0)
  }                                             //> balance: (chars: List[Char])Boolean

  balance("(hello()()())".toList)               //> res1: Boolean = true


  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty)
    {
      if(money == 0) 1
      else 0
    }
    else if(money < 0) 0
    else	countChange(money-coins.head,coins) + countChange(money,coins.tail)
  }                                 //> countChange: (money: Int, coins: List[Int])Int

  val list_coins: List[Int] = List(1,2,3,5)
  //> list_coins  : List[Int] = List(1, 2, 3, 5)
  countChange(4,list_coins)         //> res2: Int = 4

}