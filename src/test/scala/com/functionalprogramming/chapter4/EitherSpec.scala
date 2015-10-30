package com.functionalprogramming.chapter4

import org.specs2.mutable.Specification

class EitherSpec extends Specification {
  "exercises 4.6" >> {
    "map" >> {
      "returns the mapped value over Right" in {
        Right(1).map(_ + 1) ==== Right(2)
      }

      "returns itself for Left" in {
        Left("1").map((i: Int) => i + 1) ==== Left("1")
      }
    }

    "flatMap" >> {
      "returns the result of the function for Right" in {
        Right(1).flatMap(i => Left(i)) ==== Left(1)
      }

      "returns itself for Left" in {
        Left(1).flatMap((i: Int) => Right(1)) ==== Left(1)
      }
    }

    "orElse" >> {
      "returns itself for Right" in {
        Right(1).orElse(Right(2)) ==== Right(1)
      }

      "returns else branch for Left" in {
        Left(1).orElse(Right(2)) ==== Right(2)
      }
    }

    "map2" >> {
      "returns mapped value when both are Rights" in {
        Right(1).map2(Right(2))(_ + _) ==== Right(3)
      }

      "returns Left when first value is Left" in {
        Left("1").map2(Right(2))((a: Int, b: Int) => a + b) ==== Left("1")
      }

      "returns Left when second value is Left" in {
        Right(1).map2(Left("2"))((a: Int, b: Int) => a + b) ==== Left("2")
      }
    }
  }

  "exercises 4.7" >> {
    "sequence" >> {
      "returns Right of List when there are only Rights" in {
        Either.sequence(List(Right(1), Right(2), Right(3))) ==== Right(List(1, 2, 3))
      }

      "returns first Left from the List" in {
        Either.sequence(List(Right(1), Left("2"), Right(3))) ==== Left("2")
      }

      "returns Right of List when list is empty" in {
        Either.sequence(List()) ==== Right(List())
      }
    }

    "traverse" >> {
      "returns Right of List when function returns Rights" in {
        Either.traverse(List(1, 2, 3))(x => Right(x + 1)) ==== Right(List(2, 3, 4))
      }

      "returns Left when function returns Left" in {
        Either.traverse(List("1", "hello", "3"))(x => Either.Try(x.toInt)).asInstanceOf[Left[Exception]].value must beAnInstanceOf[NumberFormatException]
      }
    }
  }
}
