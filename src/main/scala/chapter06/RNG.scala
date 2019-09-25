package chapter06

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    (if (n < 0) - (n + 1)else n, nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nextRNG)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n1, nextRNG1) = rng.nextInt
    val (n2, nextRNG2) = double(nextRNG1)
    ((n1, n2), nextRNG2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n1, n2), nextRNG) = intDouble(rng)
    ((n2, n1), nextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (n1, nextRNG1) = double(rng)
    val (n2, nextRNG2) = double(nextRNG1)
    val (n3, nextRNG3) = double(nextRNG2)
    ((n1, n2, n3), nextRNG3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) {
      (List(), rng)
    } else {
      val (x, nextRNG) = rng.nextInt
      val (xs, nextNextRNG) = ints(count - 1)(nextRNG)
      (x :: xs, nextNextRNG)
    }
  }

  def intsTailRec(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, xs: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (count <= 0) {
        (xs, rng)
      } else {
        val (x, nextRNG) = rng.nextInt
        loop(count - 1, x :: xs)(nextRNG)
      }
    }

    loop(count, List())(rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def elegantDouble(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}
