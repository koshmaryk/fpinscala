package chapter05

import chapter05.Stream._

sealed trait Stream[+A] {

  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty[A])
      case _ => empty[A]
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(f: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
      case _ => empty[A]
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && b)

  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t
    )

  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t
    )

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h ,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), x) if x > 1 => Some((h(), (t(), x - 1)))
      case (Cons(h, _), 1) => Some((h(), (empty[A], 0)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
  }

  def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipAllHelper(s2)((_,_))

  private def zipAllHelper[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), None), (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(None, Some(h())), (empty[A], t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
    }

  def startsWith[A](s: Stream[A]): Boolean = ???

  def tails: Stream[Stream[A]] = ???

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty[A]
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def loop(prev: Int, next: Int): Stream[Int] =
      cons(prev, loop(next, prev + next))

    loop(0, 1)
  }

  /*
    z is an initial state
    f is a function for producing the next state and next value
  */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty[A]
      case Some((h, t)) => cons(h, unfold(t)(f))
    }

  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) {
      case (x, y) => Some((x, (y, x + y)))
    }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n + 1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  val onesViaUnfold: Stream[Int] =
    unfold(1)(_ => Some(1, 1))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
