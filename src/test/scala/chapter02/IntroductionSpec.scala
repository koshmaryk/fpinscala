package chapter02

import chapter02.Introduction._
import org.scalatest.{FlatSpec, Matchers}

class IntroductionSpec extends FlatSpec with Matchers {

  behavior of "fib"

  it should "evaluate to correct result of Fibonacci sequence" in {
    fib(0) shouldBe 0
    fib(1) shouldBe 1
    fib(2) shouldBe 1
    fib(7) shouldBe 13
    fib(12) shouldBe 144
  }

  behavior of "isSorted"

  private val lt = (x: Int, y: Int) => x < y
  it should "evaluate to correct sorted boolean value according to a comparison function" in {
    isSorted(Array(1), lt) shouldBe true
    isSorted(Array(1, 2, 3, 4, 5), lt) shouldBe true
    isSorted(Array(5, 4, 3, 2, 1), lt) shouldBe false
  }

  behavior of "curry"

  private val multiply = (x: Int, y: Int) => x * y
  it should "evaluate to same result as original uncurry function" in {
    val curried = curry(multiply)
    multiply(2, 4) shouldBe curried(2)(4)
  }

  behavior of "uncurry"

  private val divide = (x: Int) => (y: Int) => x / y
  it should "evaluate to same result as original uncurry function" in {
    val uncurried = uncurry(divide)

    divide(8)(2) shouldBe uncurried(8, 2)
  }

  behavior of "compose"

  it should "compose 2 functions" in {
    val multiply = (x: Double) => x * 10
    val divide = (y: Double) => y / 2
    val composed = compose(divide, multiply)

    composed(10) shouldBe 50
  }

}
