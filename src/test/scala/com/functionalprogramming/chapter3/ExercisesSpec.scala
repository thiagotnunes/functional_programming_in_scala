package com.functionalprogramming.chapter3

import org.specs2.mutable.Specification

class ExercisesSpec extends Specification {

  val exercises = new Exercises

  "exercises 3.1 - pattern match" >> {
    "returns 3" in {
      exercises.exercise_3_1 ==== 3
    }
  }

  "exercises 3.2 - tail" >> {
    "returns the tail of the list for non-empty list" in {
      exercises.tail(List(1, 2, 3, 4)) ==== List(2, 3, 4)
    }

    "returns nil for when the list is empty" in {
      // Could also throw an exception here
      exercises.tail(Nil) ==== Nil
    }
  }

  "exercises 3.3 - setHead" >> {
    "returns the list with the head replaced" in {
      exercises.setHead(List(1, 2, 3, 4), 5) ==== List(5, 2, 3, 4)
    }

    "returns a list with one element when it is nil" in {
      exercises.setHead(Nil, 1) ==== List(1)
    }
  }

  "exercises 3.4 - drop" >> {
    "drops n elements from the list" in {
      exercises.drop(List(1, 2, 3, 4), 2) ==== List(3, 4)
    }

    "returns nil when can not dropping more elements than the size of the list" in {
      exercises.drop(List(1, 2, 3, 4), 5) ==== Nil
    }

    "returns nil when dropping from empty list" in {
      exercises.drop(Nil, 10) ==== Nil
    }
  }

  "exercises 3.5 - dropWhile" >> {
    "drops elements from the list while predicate is true" in {
      exercises.dropWhile(List(2, 4, 5, 8), (x: Int) => x % 2 == 0) ==== List(5, 8)
    }

    "returns nil when all elements are dropped" in {
      exercises.dropWhile(List(2, 4, 6, 8), (x: Int) => x % 2 == 0) ==== Nil
    }

    "returns nil when dropping from empty list" in {
      exercises.dropWhile(Nil, (x: Int) => x % 2 == 0) ==== Nil
    }
  }

  "exercises 3.6 - init" >> {
    "returns the list without last element" in {
      exercises.init(List(1, 2, 3, 4)) ==== List(1, 2, 3)
    }

    "returns nil when the list is nil" in {
      exercises.init(Nil) ==== Nil
    }

    "returns nil when the list has only one element" in {
      exercises.init(List(1)) ==== Nil
    }
  }

  // Could not figure out a way to return early since it only stops when the list is nil
  // and the function does not have any impact on the list being iterated on
  "exercises 3.7 - product with early return on foldRight" >> todo

  "exercises 3.8 - foldRight with Nil and Cons" in {
    "returns the same list" in {
      exercises.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) ==== List(1, 2, 3)
    }
  }

  "exercises 3.9 - length" >> {
    "returns the length of the list" in {
      exercises.length(List(1, 2, 3, 4)) ==== 4
    }

    "returns 0 for nil list" in {
      exercises.length(Nil) ==== 0
    }
  }

  "exercises 3.10 - foldLeft" >> {
    "folds list" in {
      exercises.foldLeft(List(1, 2, 3, 4), 0)(_ + _) ==== 10
    }
  }

  "exercises 3.11 - sum, product and length with foldLeft" >> {
    "returns the sum of the elements" in {
      exercises.sumFoldLeft(List(1, 2, 3, 4)) ==== 10
    }

    "returns the product of the elements" in {
      exercises.productFoldLeft(List(1, 2, 3, 4)) ==== 24
    }

    "returns the length of the list" in {
      exercises.lengthFoldLeft(List(1, 2, 3, 4)) ==== 4
    }
  }

  "exercises 3.12 - reverse" >> {
    "returns the list in reverse order" in {
      exercises.reverse(List(1, 2, 3, 4)) ==== List(4, 3, 2, 1)
    }
  }

  "exercises 3.13 - foldLeft x foldRight" >> todo

  "exercises 3.14 - append" >> {
    "appends an element to the list" in {
      exercises.append(List(1, 2, 3), 4) ==== List(1, 2, 3, 4)
    }
  }

  "exercises 3.15 - flatten" >> {
    "concatenates lists of list into single list" in {
      exercises.flatten(List(List(1, 2), List(3, 4))) ==== List(1, 2, 3, 4)
    }
  }
}
