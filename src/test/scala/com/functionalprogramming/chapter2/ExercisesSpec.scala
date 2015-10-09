package com.functionalprogramming.chapter2

import org.specs2.mutable.Specification

class ExercisesSpec extends Specification {
  val subject = new Exercises

  "exercise 2.1 - fib" >> {
    "returns 0 for 0" in {
      subject.fib(0) ==== 0
    }

    "returns 1 for 1" in {
      subject.fib(1) ==== 1
    }

    "returns 1 for 2" in {
      subject.fib(2) ==== 1
    }

    "returns 13 for 6" in {
      subject.fib(7) ==== 13
    }
  }
}
