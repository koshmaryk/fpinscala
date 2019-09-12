package chapter04

import scala.{Either => _}

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

  def map2both[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] = (this , b) match {
    case (Right(aa), Right(bb)) => Right(f(aa, bb))
    case (Left(aa), Left(bb)) => Left(List(aa, bb))
    case (Left(aa), _) => Left(List(aa))
    case (_, Left(bb)) => Left(List(bb))
  }
}

object Either {

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match {
    case Nil => Right(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))

  }

  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]
