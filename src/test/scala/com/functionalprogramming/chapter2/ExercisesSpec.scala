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

  "exercise 2.2 - isSorted" >> {
    "returns true when array is sorted accordingly" in {
      val array = Array(5,4,3,2,1)
      subject.isSorted(array, (a: Int, b: Int) => a > b) ==== true
    }

    "returns true when array contains only one element" in {
      val array = Array(1)
      subject.isSorted(array, (a: Int, b: Int) => a > b) ==== true
    }

    "returns true when array is empty" in {
      val array = Array()
      subject.isSorted(array, (a: Int, b: Int) => a > b) ==== true
    }

    "returns false when array is not sorted accordingly" in {
      val array = Array(1,2,3,4,5)
      subject.isSorted(array, (a: Int, b: Int) => a > b) ==== false
    }
  }
}
