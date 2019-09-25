package chapter06

import org.scalatest.{FunSpec, Matchers}

class StateAutomatonSpec extends FunSpec with Matchers {

  it("test that machine that is out of candy ignores all inputs") {
    val machine = Machine(locked = true, candies = 0, coins = 0)

    val inputs = List(Coin, Turn)

    val result = StateAutomaton.simulateMachine(inputs).run(machine)

    result shouldBe ((0,0), machine)
  }

  it("test that inserting a coin into a locked machine unlocks it if candies remain") {
    val machine = Machine(locked = true, candies = 5, coins = 0)

    val inputs = List(Coin)

    val result = StateAutomaton.simulateMachine(inputs).run(machine)

    result shouldBe ((5,1), Machine(locked = false, 5, 1))
  }

  it("test that turning the knob on an unlocked machine will cause it to dispense candy and become locked") {
    val machine = Machine(locked = false, candies = 5, coins = 1)

    val inputs = List(Turn)

    val result = StateAutomaton.simulateMachine(inputs).run(machine)

    result shouldBe ((4, 1), Machine(locked = true, 4, 1))
  }

  it("test that turning the knob on a locked machine does nothing") {
    val machine = Machine(locked = true, candies = 5, coins = 1)

    val inputs = List(Turn)

    val result = StateAutomaton.simulateMachine(inputs).run(machine)

    result shouldBe ((5, 1), machine)
  }

  it("test that inserting a coin into an unlocked machine does nothing") {
    val machine = Machine(locked = false, candies = 5, coins = 1)

    val inputs = List(Coin)

    val result = StateAutomaton.simulateMachine(inputs).run(machine)

    result shouldBe ((5, 1), machine)
  }

  it("test that simulateMachine returns expected final state for sequence of inputs") {
    val machine = Machine(locked = true, candies = 2, coins = 0)

    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn)

    val result = StateAutomaton.simulateMachine(inputs).run(machine)

    result shouldBe ((0, 2), Machine(locked = true, candies = 0, coins = 2))
  }

  it("test that simulateMachine returns expected final state") {
    val machine = Machine(locked = true, candies = 2, coins = 0)

    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn)

    val result = StateAutomaton.simulateMachine(inputs).run(machine)

    result shouldBe ((0, 2), Machine(locked = true, candies = 0, coins = 2))
  }

}
