package chapter05

import chapter05.Stream._
import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  behavior of "toList"

  it should "convert to scala List type" in {
    Stream(1, 2, 3).toList shouldBe List(1, 2, 3)
  }

  behavior of "take"

  it should "take first n elements" in {
    Stream(1, 2, 3).take(2).toList shouldBe  List(1, 2)
  }

  behavior of "drop"

  it should "drop first n elements" in {
    Stream(1, 2, 3).drop(2).toList shouldBe  List(3)
  }

  behavior of "takeWhile"

  it should "take elements if condition evaluates to true" in {
    Stream(1, 2, 3).takeWhile(_ < 3).toList shouldBe List(1, 2)
  }

  behavior of "forAll"

  it should "evaluate to true if condition is true for all elements" in {
    Stream(1, 2, -3).forAll(_ > 0) shouldBe false
  }

  behavior of "takeWhileViaFoldRight"

  it should "take via foldRight elements if condition evaluates to true" in {
    Stream(1, 2, 3).takeWhileViaFoldRight(_ < 3).toList shouldBe List(1, 2)
  }

  behavior of "append"

  it should "append one Stream to another" in {
    Stream(1, 2, 3).append(Stream(4, 5, 6)).toList shouldBe List(1, 2, 3, 4, 5, 6)
  }

  behavior of "flatMap"

  it should "evaluate to same element + (element + 1) for each of it" in {
    Stream(1, 2, 3).flatMap(x => Stream(x, x + 1)).toList shouldBe List(1, 2, 2, 3, 3, 4)
  }

  behavior of "constant"

  it should "define infinite Stream of same element" in {
    constant(1).take(3).toList shouldBe List(1, 1, 1)
  }

  behavior of "from"

  it should "define infinite Stream of incremental element" in {
    from(1).take(3).toList shouldBe List(1, 2, 3)
  }

  behavior of "fibonacci numbers"

  it should "define fibonacci numbers with Stream" in {
    fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  behavior of "unfold"

  it should "define infinite Stream via general stream-building function" in {
    unfold(0)(x => Some(x, x + 1)).take(3).toList shouldBe List(0, 1, 2)
  }

  behavior of "fibonacci numbers via unfold"

  it should "define fibonacci numbers with Stream via unfold" in {
    fibsViaUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  behavior of "fromViaUnfold"

  it should "define infinite Stream of incremental element" in {
    fromViaUnfold(1).take(3).toList shouldBe List(1, 2, 3)
  }

  behavior of "fromViaUnfold"

  it should "define infinite Stream of same element via unfold" in {
    constantViaUnfold(1).take(3).toList shouldBe List(1, 1, 1)
  }

  behavior of "onesViaUnfold"

  it should "define infinite Stream of 1 via unfold" in {
    onesViaUnfold.take(3).toList shouldBe List(1, 1, 1)
  }

  behavior of "mapViaUnfold"

  it should "map elements of Stream via unfold" in {
    Stream(1, 2, 3).mapViaUnfold(_ + 1).toList shouldBe List(2, 3, 4)
  }

  behavior of "takeViaUnfold"

  it should "take first n elements via unfold" in {
    Stream(1, 2, 3).takeViaUnfold(2).toList shouldBe  List(1, 2)
  }

  behavior of "takeWhileViaUnfold"

  it should "take elements if condition evaluates to true" in {
    Stream(1, 2, 3).takeWhileViaUnfold(_ < 3).toList shouldBe List(1, 2)
  }

  behavior of "zipWith"

  it should "evaluate to correct stream with concat string elements of both streams" in {
    Stream("1", "2", "3").zipWith(Stream("3", "4", "5"))(_ + " + " + _).toList shouldBe List("1 + 3", "2 + 4", "3 + 5")
  }

  behavior of "zipAll"

  it should "evaluate to correct stream of tuples of Some or None" in {
    Stream(1, 2, 3).zipAll(Stream(4, 5)).toList shouldBe List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), None))
    Stream(1, 2).zipAll(Stream(3, 4, 5)).toList shouldBe List((Some(1), Some(3)), (Some(2), Some(4)), (None, Some(5)))
  }
}
