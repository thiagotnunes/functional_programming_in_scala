package com.functionalprogramming.chapter4

import org.specs2.mutable.Specification

class OptionExercisesSpec extends Specification {

  val exercises = new OptionExercises

  "exercises 4.2" >> {
    "variance" >> {
      "returns Some variance when list is not empty" in {
        exercises.variance(List(1.0, 2.0, 3.0, 4.0)) ==== Some(1.25)
      }

      "returns None when list is empty" in {
        exercises.variance(List.empty) ==== None
      }
    }
  }

  "exercises 4.3" >> {
    "returns Some mapped value when both options are defined" in {
      exercises.map2(Some(1), Some(2))(_ + _) ==== Some(3)
    }

    "returns None when first option is not defined" in {
      exercises.map2(None, Some(1))((a: Int, b: Int) => a + b) ==== None
    }

    "returns None when second option is not defined" in {
      exercises.map2(Some(1), None)((a: Int, b: Int) => a + b) ==== None
    }

    "returns None when both option is not defined" in {
      exercises.map2(None, None)((a: Int, b: Int) => a + b) ==== None
    }
  }

  "exercises 4.4" >> {
    "returns Some List when all options are defined" in {
      Option.sequence(List(Some(1), Some(2), Some(3))) ==== Some(List(1, 2, 3))
    }

    "returns None when one option is not defined" in {
      Option.sequence(List(Some(1), None, Some(3))) ==== None
    }

    "returns Some empty List when list is empty" in {
      Option.sequence(List()) ==== Some(List())
    }
  }

  "exercises 4.5" >> {
    "returns Some List of parsed ints" in {
      Option.traverse(List("1", "2", "3"))(x => Option.Try(x.toInt)) ==== Some(List(1, 2, 3))
    }

    "returns None when there is an error parsing one of the elements" in {
      Option.traverse(List("1", "hello", "3"))(x => Option.Try(x.toInt)) ==== None
    }

    "returns Some List when list is empty" in {
      Option.traverse(List())((x: Int) => Some(x)) ==== Some(List())
    }
  }

}
