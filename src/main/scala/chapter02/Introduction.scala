package chapter02

import scala.annotation.tailrec

object Introduction {

  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)

    loop(n,0,1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else false

    loop(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    x => y => f(x, y)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (x, y) => f(x)(y)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    x => f(g(x))
}
