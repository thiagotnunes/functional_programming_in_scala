package com.functionalprogramming.chapter6

import org.specs2.mutable.Specification

class MachineSimulatorSpec extends Specification {
  private val simulator = new MachineSimulator

  "inserting coin" >> {
    "unlocks machine when machine is locked" in {
      simulator.simulateMachine(List(Coin)).run(Machine(true, 5, 10))._2 ==== Machine(false, 5, 11)
    }

    "does nothing when machine is already unlocked" in {
      simulator.simulateMachine(List(Coin)).run(Machine(false, 10, 10))._2 ==== Machine(false, 10, 10)
    }

    "does nothing when machine has no candies left" in {
      simulator.simulateMachine(List(Coin)).run(Machine(true, 0, 20))._2 ==== Machine(true, 0, 20)
    }
  }

  "turning the knob" >> {
    "dispenses candy and locks on an unlocked machine with some candy left" in {
      simulator.simulateMachine(List(Turn)).run(Machine(false, 1, 10))._2 ==== Machine(true, 0, 10)
    }

    "does nothing when machine is locked" in {
      simulator.simulateMachine(List(Turn)).run(Machine(true, 10, 10))._2 ==== Machine(true, 10, 10)
    }

    "does nothing when machine has no candies left" in {
      simulator.simulateMachine(List(Turn)).run(Machine(false, 0, 20))._2 ==== Machine(false, 0, 20)
    }
  }

  "several commands" >> {
    "insert coin and turn the knob" in {
      simulator.simulateMachine(List(Coin, Turn)).run(Machine(true, 10, 10))._2 ==== Machine(true, 9, 11)
    }
  }
}
