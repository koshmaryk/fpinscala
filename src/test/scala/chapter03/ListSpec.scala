package chapter03

import chapter03.List._
import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {

  behavior of "tail"

  it should "evaluate to correct tail" in {
    tail(List(1)) shouldBe Nil
    tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  behavior of "setHead"

  it should "evaluate to correct list with updated head" in {
    setHead(Nil, 1) shouldBe List(1)
    setHead(List(1, 2, 3), 0) shouldBe List(0, 2, 3)
  }

  behavior of "drop"

  it should "evaluate to correct updated list without dropped elements" in {
    drop(Nil, 1) shouldBe Nil
    drop(List(1, 2, 3, 4, 5), 2) shouldBe List(3, 4, 5)
    drop(List(1, 2, 3, 4, 5), 0) shouldBe List(1, 2, 3, 4, 5)
  }

  behavior of "dropWhile"

  it should "evaluate to correct updated list without dropped elements according to a comparison function" in {
    dropWhile(List(1, 2, 3), (x: Int) => x < 3) shouldBe List(3)
    dropWhile(List(1, 2, 3), (x: Int) => x < 5) shouldBe Nil
  }

  behavior of "init"

  it should "evaluate to correct updated list without dropped elements according to a comparison function" in {
    init(List(1)) shouldBe Nil
    init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
  }

  behavior of "lengthOf"

  it should "evaluate to correct list's length" in {
    lengthOf(Nil) shouldBe 0
    lengthOf(List(1, 2, 3)) shouldBe 3
  }

  behavior of "foldLeft"

  it should "evaluate to correct sum of list elements" in {
    foldLeft(Nil: List[Int], 0)(_ + _) shouldBe 0
    foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
  }

  behavior of "sum2"

  it should "evaluate to correct sum" in {
    sum2(Nil) shouldBe 0
    sum2(List(1, 2, 3)) shouldBe 6
  }

  behavior of "product2"

  it should "evaluate to correct product" in {
    product2(Nil) shouldBe 1.0
    product2(List(1.0, 2.0, 3.0)) shouldBe 6.0
  }

  behavior of "lengthOf2"

  it should "evaluate to correct list's length" in {
    lengthOf2(Nil) shouldBe 0
    lengthOf2(List(1, 2, 3)) shouldBe 3
  }

  behavior of "reverse"

  it should "evaluate to correct reversed list" in {
    reverse(Nil) shouldBe Nil
    reverse(List(1)) shouldBe List(1)
    reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  behavior of "foldLeft2"

  it should "behave as foldLeft" in {
    foldLeft(List(1, 2, 3), 1.0)(_ * _) shouldBe foldLeft2(List(1, 2, 3), 1.0)(_ * _)
  }

  behavior of "foldRight2"

  it should "behave as foldRight" in {
    foldRight(List(1, 2, 3), 1.0)(_ * _) shouldBe foldRight2(List(1, 2, 3), 1.0)(_ * _)
  }

  behavior of "append"

  it should "evaluate to correct list consists of two" in {
    append(Nil, Nil) shouldBe Nil
    append(Nil, List(1)) shouldBe List(1)
    append(List(1, 2), List(3, 4, 5)) shouldBe List(1, 2, 3, 4, 5)
  }

  behavior of "concatLists"

  it should "evaluate to correct concatenated list" in {
    concatLists(List(List(1, 2), List(3, 4), List(5))) shouldBe List(1, 2, 3, 4, 5)
  }

  behavior of "addOne"

  it should "evaluate to correct list with ++ elements" in {
    addOne(Nil) shouldBe Nil
    addOne(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  behavior of "doubleToString"

  it should "evaluate to correct list with string elements" in {
    doubleToString(Nil) shouldBe Nil
    doubleToString(List(1.0, 2.0, 3.0)) shouldBe List("1.0", "2.0", "3.0")
  }

  behavior of "map"

  it should "evaluate to correct list with * 2 elements" in {
    map(Nil: List[Int])(_ * 2) shouldBe Nil
    map(List(1, 2, 3))(_ * 2) shouldBe List(2, 4, 6)
  }

  behavior of "filter"

  it should "evaluate to correct list with elements that match predicate" in {
    filter(Nil: List[Int])(_ > 2) shouldBe Nil
    filter(List(1, 2, 3))(_ > 2) shouldBe List(3)
  }

  behavior of "flatMap"

  it should "evaluate to correct list with elements that match predicate" in {
    flatMap(Nil: List[Int])(i => List(i, i)) shouldBe Nil
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  behavior of "filter2"

  it should "evaluate2 to correct list with elements that match predicate" in {
    filter2(Nil: List[Int])(_ > 2) shouldBe Nil
    filter2(List(1, 2, 3))(_ > 2) shouldBe List(3)
  }

  behavior of "addInt"

  it should "evaluate to correct list with added elements of both lists" in {
    addInt(Nil: List[Int], List(1, 2, 3)) shouldBe List(1, 2, 3)
    addInt(List(1, 2, 3), Nil: List[Int]) shouldBe List(1, 2, 3)
    addInt(List(1, 2, 3), List(3, 4, 5)) shouldBe List(4, 6, 8)
  }

  behavior of "zipWith"

  it should "evaluate to correct list with concat string elements of both lists" in {
    zipWith(Nil: List[String], List("1", "2", "3"))(_ + " + " + _) shouldBe List("1", "2", "3")
    zipWith(List("1", "2", "3"), Nil: List[Int])(_ + " + " + _) shouldBe List("1", "2", "3")
    zipWith(List("1", "2", "3"), List("3", "4", "5"))(_ + " + " + _) shouldBe List("1 + 3", "2 + 4", "3 + 5")
  }
}
