package funsets

import common._

object Funsets {

  type Set = Int => Boolean

  def singletonSet(elem: Int): Set = {
    (x: Int) => x == elem
  }                                               //> singletonSet: (elem: Int)Int => Boolean

  def nullSet: Set = {
    (x: Int) => false
  }                                               //> nullSet: => Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)
  //> contains: (s: Int => Boolean, elem: Int)Boolean

  def union(s: Set, t: Set): Set = {
    (x: Int) => contains(s,x) || contains(t,x)
  }                                               //> union: (s: Int => Boolean, t: Int => Boolean)Int => Boolean

  def intersect(s: Set, t: Set) = {
    (x: Int) => contains(s,x) && contains(t,x)
  }                                               //> intersection: (s: Int => Boolean, t: Int => Boolean)Int => Boolean

  def differ(s: Set, t: Set) = {
    (x: Int) => contains(s,x) && !contains(t,x)
  }                                               //> difference: (s: Int => Boolean, t: Int => Boolean)Int => Boolean

  def filter(s: Set, p: Int => Boolean): Set = {
    (x: Int) => contains(((x: Int) => contains(s,x)&&p(x)), x)
  }                                               //> filter: (s: Int => Boolean, p: Int => Boolean)Int => Boolean

  val bound = 1000

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if(a == bound+1) true
      else if(a <= bound && contains(s,a)) p(a) && iter(a+1)
      else iter(a+1)
    }
    iter(-bound)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = {
    def q(p: Int => Boolean): Int => Boolean = !p(_)
    if(forall(s,q(p))) false
    else true
  }

  def map(s: Set, f: Int => Int): Set = {
    def st: Set = nullSet
    def loop(a: Int, nst: Set): Set = {
      if(a == 1001){
        nst
      }
      else if(contains(s,a)) {
        loop(a+1,union(nst,singletonSet(f(a))))
      }
      else loop(a+1,nst)
    }
    loop(-1000,st)
  }                                               //> map: (s: Int => Boolean, f: Int => Int)Int => Boolean

  def toString(s: Set): String = {
    val xs = for (i <- -1000 to 1000 if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }                                               //> toString: (s: Int => Boolean)String

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }                                               //> printSet: (s: Int => Boolean)Unit


}
