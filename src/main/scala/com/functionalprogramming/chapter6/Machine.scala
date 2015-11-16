package com.functionalprogramming.chapter6

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

class MachineSimulator {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- State.sequence(inputs.map((State.modify[Machine] _).compose(update)))
      newMachine <- State.get
    } yield {
      (newMachine.coins, newMachine.candies)
    }
  }

  def update = (input: Input) => (machine: Machine) => {
    (input, machine) match {
      case (_, Machine(_, 0, _)) => machine
      case (Coin, Machine(false, _, _)) => machine
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      case (Turn, Machine(true, _, _)) => machine
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    }
  }
}