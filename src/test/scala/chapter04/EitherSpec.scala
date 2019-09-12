package chapter04

import chapter04.Either._
import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {

  behavior of "sequence"

  it should "evaluate to >>> correct value after applying sequence" in {
    sequence(List(Left(1))) shouldBe Left(1)
    sequence(List(Left(1), Right(1))) shouldBe Left(1)
    sequence(List(Right(1))) shouldBe Right(List(1))
    sequence(List(Right(1), Right(2))) shouldBe Right(List(1, 2))
  }

  behavior of "traverse"

  it should "evaluate to >>> correct value after applying traverse" in {
    traverse(List(1))(v => if (v > 0) Right(v) else Left("Error")) shouldBe Right(List(1))
    traverse(List(1, 0))(v => if (v > 0) Right(v) else Left("Error")) shouldBe Left("Error")
    traverse(List(1, 2))(v => if (v > 0) Right(v) else Left("Error")) shouldBe Right(List(1, 2))
  }

  behavior of "map2both"

  it should "be fail-safe" in {
    case class Person(name: Name, age: Age)
    sealed case class Name(value: String)
    sealed case class Age(value: Int)

    def mkName(name: String): Either[String, Name] =
      if (name == "" || name == null) Left("Name is empty.")
      else Right(Name(name))

    def mkAge(age: Int): Either[String, Age] =
      if (age < 0) Left("Age is out of range.")
      else Right(Age(age))

    def mkPerson(name: String, age: Int): Either[List[String], Person] =
      mkName(name).map2both(mkAge(age))(Person)


    assert(mkPerson("Yevhen", 21) == Right(Person(Name("Yevhen"), Age(21))))
    assert(mkPerson("", 21) == Left(List("Name is empty.")))
    assert(mkPerson("Yevhen", -1) == Left(List("Age is out of range.")))
    assert(mkPerson("", -1) == Left(List("Name is empty.", "Age is out of range.")))

  }

}
