package chapter04

import chapter04.Option._
import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  behavior of "map"

  it should "evaluate to correct >>> Option after applying map" in {
    Some(1).map(_ * 2) shouldBe Some(2)
  }

  behavior of "flatMap"

  it should "evaluate to correct >>> Some of Some after applying flatMap" in {
    Some(1).map(v => Some(v * 2)) shouldBe Some(Some(2))
  }

  behavior of "getOrElse"

  it should "evaluate to correct >>> int" in {
    Some(2).getOrElse(0) shouldBe 2
    None.getOrElse(0) shouldBe 0
  }

  behavior of "orElse"

  it should "evaluate to correct >>> else Option" in {
    Some(1).orElse(Some(2)) shouldBe Some(1)
    None.orElse(Some(2)) shouldBe Some(2)
  }

  behavior of "filter"

  it should "evaluate to correct >>> filtered value" in {
    Some(1).filter(_ > 0) shouldBe Some(1)
    Some(1).filter(_ < 0) shouldBe None
  }

  behavior of "mean"

  it should "evaluate to >>> correct mean value after applying mean" in {
    mean(Nil) shouldBe None
    mean(Seq(1.0, 2.0, 3.0)) shouldBe Some(2.0)
  }

  behavior of "variance"

  it should "evaluate to >>> correct value after applying variance" in {
    variance(Nil) shouldBe None
    variance(Seq(2.0, 3.0)) shouldBe Some(0.25)
  }

  behavior of "map2"

  it should "evaluate to >>> correct value after applying map2" in {
    map2(Some(1), Some(2))(_ + _) shouldBe Some(3)
    map2(Some(1), None)(_ + _) shouldBe None
  }

  behavior of "sequence"

  it should "evaluate to >>> correct value after applying sequence" in {
    sequence(List(None)) shouldBe None
    sequence(List(None, Some(1))) shouldBe None
    sequence(List(Some(1))) shouldBe Some(List(1))
    sequence(List(Some(1), Some(2))) shouldBe Some(List(1, 2))
  }

  behavior of "traverse"

  it should "evaluate to >>> correct value after applying traverse" in {
    traverse(List("1", "2", "3"))(v => Some(v.toInt)) shouldBe Some(List(1, 2, 3))
  }
}
