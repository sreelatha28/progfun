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

  def intersection(s: Set, t: Set) = {
    (x: Int) => contains(s,x) && contains(t,x)
  }                                               //> intersection: (s: Int => Boolean, t: Int => Boolean)Int => Boolean

  def difference(s: Set, t: Set) = {
    (x: Int) => contains(s,x) && !contains(t,x)
  }                                               //> difference: (s: Int => Boolean, t: Int => Boolean)Int => Boolean

  def filter(s: Set, p: Int => Boolean): Set = {
    (x: Int) => contains(((x: Int) => contains(s,x)&&p(x)), x)
  }                                               //> filter: (s: Int => Boolean, p: Int => Boolean)Int => Boolean

  def x: Set = union(union(singletonSet(1),singletonSet(2)),singletonSet(5))
  //> x: => Int => Boolean

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if(a == 1001) true
      else if(a <= 1000 && contains(s,a)) p(a) && iter(a+1)
      else iter(a+1)
    }
    iter(-1000)
  }                                               //> forall: (s: Int => Boolean, p: Int => Boolean)Boolean
  forall(x, y => (y>=1))                          //> res0: Boolean = true

  def exists(s: Set, p: Int => Boolean): Boolean = {
    def q(p: Int => Boolean): Int => Boolean = {
      (x: Int) => !p(x)
    }
    if(forall(s,q(p))) false
    else true
  }                                               //> exists: (s: Int => Boolean, p: Int => Boolean)Boolean

  exists(x,y => (y==2))                           //> res1: Boolean = true

  def map(s: Set, f: Int => Int): Set = {
    def st: Set = nullSet
    def mp(a: Int, nst: Set): Set = {
      if(a == 1001){
        nst
      }
      else if(contains(s,a)) {
        mp(a+1,union(nst,singletonSet(f(a))))
      }
      else mp(a+1,nst)
    }
    mp(-1000,st)
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

  contains(x,2)                                   //> res2: Boolean = true
  contains(map(x,y => y*y),20)              //> res3: Boolean = false
  printSet(x)                               //> {1,2,5}
}
