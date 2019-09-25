package chapter06

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object StateAutomaton {

  import State._

  def update(input: Input)(machine: Machine): Machine =
    (input, machine) match {
      case (_, Machine(_, 0, _)) =>
        println("A machine that’s out of candy ignores all inputs.")
        machine
      case (Coin, Machine(true, candies, coins)) =>
        println("Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.")
        Machine(locked = false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) =>
        println("Turning the knob on an unlocked machine will cause it to dispense candy and become locked.")
        Machine(locked = true, candies - 1, coins)
      case (Turn, Machine(true, _, _)) =>
        println("Turning the knob on a locked machine does nothing.")
        machine
      case (Coin, Machine(false, _, _)) =>
        println("Inserting a coin into an unlocked machine does nothing.")
        machine
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.candies, s.coins)
}
