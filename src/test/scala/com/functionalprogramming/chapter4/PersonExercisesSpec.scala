package com.functionalprogramming.chapter4

import org.specs2.mutable.Specification

class PersonExercisesSpec extends Specification {

  val exercises = new PersonExercises

  "returns both errors" in {
    exercises.mkPerson(null, -1) ==== Failure(List("Name is empty.", "Age is out of range."))
  }
}
