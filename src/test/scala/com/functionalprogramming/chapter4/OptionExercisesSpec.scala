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
}
