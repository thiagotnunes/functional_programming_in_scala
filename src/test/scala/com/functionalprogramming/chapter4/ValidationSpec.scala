package com.functionalprogramming.chapter4

import org.specs2.mutable.Specification

class ValidationSpec extends Specification {
  "exercises 4.8" >> {
    "map" >> {
      "returns the mapped value over Success" in {
        Success(1).map(_ + 1) ==== Success(2)
      }

      "returns itself for Failure" in {
        Failure("1").map((i: Int) => i + 1) ==== Failure("1")
      }
    }

    "flatMap" >> {
      "returns the result of the function for Success" in {
        Success(1).flatMap(i => Failure(i)) ==== Failure(1)
      }

      "returns itself for Failure" in {
        Failure(1).flatMap((i: Int) => Success(1)) ==== Failure(1)
      }
    }

    "orElse" >> {
      "returns itself for Success" in {
        Success(1).orElse(Success(2)) ==== Success(1)
      }

      "returns else branch for Failure" in {
        Failure(1).orElse(Success(2)) ==== Success(2)
      }
    }

    "map2" >> {
      "returns mapped value when both are Success" in {
        Success(1).map2(Success(2))(_ + _) ==== Success(3)
      }

      "returns Failure when first value is Failure" in {
        Failure("1").map2(Success(2))((a: Int, b: Int) => a + b) ==== Failure(List("1"))
      }

      "returns Failure when second value is Failure" in {
        Success(1).map2(Failure("2"))((a: Int, b: Int) => a + b) ==== Failure(List("2"))
      }

      "returns both Failures when both are Failures" in {
        Failure("1").map2(Failure("2"))((a: Int, b: Int) => a + b) ==== Failure(List("1", "2"))
      }
    }

    "sequence" >> {
      "returns Success of List when there are only Successs" in {
        Validation.sequence(List(Success(1), Success(2), Success(3))) ==== Success(List(1, 2, 3))
      }

      "returns all Failures from the List" in {
        Validation.sequence(List(Success(1), Failure("2"), Failure("3"))) ==== Failure(List("2", "3"))
      }

      "returns Success of List when list is empty" in {
        Validation.sequence(List()) ==== Success(List())
      }
    }

    "traverse" >> {
      "returns Success of List when function returns Success" in {
        Validation.traverse(List(1, 2, 3))(x => Success(x + 1)) ==== Success(List(2, 3, 4))
      }

      "returns Failure when function returns Failure" in {
        Validation.traverse(List("1", "hello", "failure"))(x => Failure(x + " failure")) ==== Failure(List("1 failure", "hello failure", "failure failure"))
      }
    }
  }
}
