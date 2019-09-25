package chapter06

import chapter06.RNG.SimpleRNG
import org.scalatest.{FlatSpec, Matchers}

class StateSpec extends FlatSpec with Matchers {

  behavior of "State.unit"

  it should "evaluate to a new state containing the specified value" in {
   State.unit[RNG, Int](1).run(SimpleRNG(42))._1 shouldBe 1
  }

  behavior of "State.map()"

  it should "evaluate a new state with a value modified by the specified function: map" in {
    val initialState = State.unit[RNG, Int](1)

    initialState.map(_ + 1).run(SimpleRNG(42))._1 shouldBe 2
  }

  behavior of "State.map2()"

  it should "combine a result of two actions wrapped in State" in {
    State.unit[RNG, Int](1).map2(State.unit[RNG, Int](1))((a: Int, b: Int) => a + b).run(SimpleRNG(42))._1 shouldBe 2
  }

  behavior of "State.flatMap()"

  it should "evaluate a new state with a value modified by the specified function: flatMap" in {
    val initialState = State.unit[RNG, Int](1)

    initialState.flatMap(a => State.unit(a + 1)).run(SimpleRNG(42))._1 shouldBe 2
  }

  behavior of "State.sequence()"

  it should "combines a list of actions wrapped in State with sequence" in {
    val list = List(State.unit[RNG, Int](1), State.unit[RNG, Int](2), State.unit[RNG, Int](3))
    State.sequence(list).run(SimpleRNG(42))._1 shouldBe List(1, 2, 3)
  }

  behavior of "State.sequenceViaFoldRight()"

  it should "combines a list of actions wrapped in State with sequenceViaFoldRight" in {
    val list = List(State.unit[RNG, Int](1), State.unit[RNG, Int](2), State.unit[RNG, Int](3))
    State.sequenceViaFoldRight(list).run(SimpleRNG(42))._1 shouldBe List(1, 2, 3)
  }

}
