package com.functionalprogramming.chapter4

import org.specs2.mutable.Specification

class EitherExercisesSpec extends Specification {
  val exercises = new EitherExercises

  "exercises 4.8" >> {
    "map2 with error accumulation" >> {
      exercises.mkPerson(null, -1) ==== Left(List("Name is empty.", "Age is out of range."))
    }
  }
}
