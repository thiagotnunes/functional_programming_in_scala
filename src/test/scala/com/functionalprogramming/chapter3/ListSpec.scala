package com.functionalprogramming.chapter3

import org.specs2.mutable.Specification

class ListSpec extends Specification {

  val lists = new Lists

  "exercises 3.1 - pattern match" >> {
    "returns 3" in {
      lists.exercise_3_1 ==== 3
    }
  }

  "exercises 3.2 - tail" >> {
    "returns the tail of the list for non-empty list" in {
      List.tail(List(1, 2, 3, 4)) ==== List(2, 3, 4)
    }

    "returns nil for when the list is empty" in {
      // Could also throw an exception here
      List.tail(Nil) ==== Nil
    }
  }

  "exercises 3.3 - setHead" >> {
    "returns the list with the head replaced" in {
      List.setHead(List(1, 2, 3, 4), 5) ==== List(5, 2, 3, 4)
    }

    "returns a list with one element when it is nil" in {
      List.setHead(Nil, 1) ==== List(1)
    }
  }

  "exercises 3.4 - drop" >> {
    "drops n elements from the list" in {
      List.drop(List(1, 2, 3, 4), 2) ==== List(3, 4)
    }

    "returns nil when can not dropping more elements than the size of the list" in {
      List.drop(List(1, 2, 3, 4), 5) ==== Nil
    }

    "returns nil when dropping from empty list" in {
      List.drop(Nil, 10) ==== Nil
    }
  }

  "exercises 3.5 - dropWhile" >> {
    "drops elements from the list while predicate is true" in {
      List.dropWhile(List(2, 4, 5, 8), (x: Int) => x % 2 == 0) ==== List(5, 8)
    }

    "returns nil when all elements are dropped" in {
      List.dropWhile(List(2, 4, 6, 8), (x: Int) => x % 2 == 0) ==== Nil
    }

    "returns nil when dropping from empty list" in {
      List.dropWhile(Nil, (x: Int) => x % 2 == 0) ==== Nil
    }
  }

  "exercises 3.6 - init" >> {
    "returns the list without last element" in {
      List.init(List(1, 2, 3, 4)) ==== List(1, 2, 3)
    }

    "returns nil when the list is nil" in {
      List.init(Nil) ==== Nil
    }

    "returns nil when the list has only one element" in {
      List.init(List(1)) ==== Nil
    }
  }

  "exercises 3.7 - product with early return on foldRight" >> {
    // Could not figure out a way to return early since it only stops when the list is nil
    // and the function does not have any impact on the list being iterated on
    "bogus test" in {
      1 ==== 1
    }
  }

  "exercises 3.8 - foldRight with Nil and Cons" in {
    "returns the same list" in {
      List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) ==== List(1, 2, 3)
    }
  }

  "exercises 3.9 - length" >> {
    "returns the length of the list" in {
      List.length(List(1, 2, 3, 4)) ==== 4
    }

    "returns 0 for nil list" in {
      List.length(Nil) ==== 0
    }
  }

  "exercises 3.10 - foldLeft" >> {
    "folds list" in {
      List.foldLeft(List(1, 2, 3, 4), 0)(_ + _) ==== 10
    }
  }

  "exercises 3.11 - sum, product and length with foldLeft" >> {
    "returns the sum of the elements" in {
      List.sumFoldLeft(List(1, 2, 3, 4)) ==== 10
    }

    "returns the product of the elements" in {
      List.productFoldLeft(List(1, 2, 3, 4)) ==== 24
    }

    "returns the length of the list" in {
      List.lengthFoldLeft(List(1, 2, 3, 4)) ==== 4
    }
  }

  "exercises 3.12 - reverse" >> {
    "returns the list in reverse order" in {
      List.reverse(List(1, 2, 3, 4)) ==== List(4, 3, 2, 1)
    }
  }

  "exercises 3.13 - foldLeft x foldRight" >> {
    "folds left using fold right" in {
      List.foldLeftViaFoldRight(List(1, 2, 3, 4), Nil: List[Int])((tail, head) => Cons(head, tail)) ==== List(4, 3, 2, 1)
    }

    "folds right using fold left" in {
      List.foldRightViaFoldLeft(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)) ==== List(1, 2, 3, 4)
    }
  }

  "exercises 3.14 - append" >> {
    "appends an element to the list" in {
      List.append(List(1, 2, 3), 4) ==== List(1, 2, 3, 4)
    }
  }

  "exercises 3.15 - flatten" >> {
    "concatenates list of list into single list" in {
      List.flatten(List(List(1, 2), List(3, 4))) ==== List(1, 2, 3, 4)
    }
  }

  "exercises 3.16 - increment list" >> {
    "adds 1 to each element of the list" in {
      List.increment(List(1, 2, 3, 4)) ==== List(2, 3, 4, 5)
    }
  }

  "exercises 3.17 - stringify list" >> {
    "stringify each element of the list" in {
      List.stringify(List(1.0, 2.0, 3.0, 4.0)) ==== List("1.0", "2.0", "3.0", "4.0")
    }
  }

  "exercises 3.18 - map" >> {
    "maps each element of the list" in {
      List.map(List(1, 2, 3, 4))(x => x - 1) ==== List(0, 1, 2, 3)
    }
  }

  "exercises 3.19 - filter" >> {
    "removes elements that do not comply to predicate" in {
      List.filter(List(1, 2, 3, 4))(x => x % 2 == 0) ==== List(2, 4)
    }
  }

  "exercises 3.20 - flatMap" >> {
    "maps each element and flattens" in {
      List.flatMap(List(1, 2, 3))(x => List(x, x)) ==== List(1, 1, 2, 2, 3, 3)
    }
  }

  "exercises 3.21 - filterViaFlatMap" >> {
    "removes elements that do not comply to predicate" in {
      List.filterViaFlatMap(List(1, 2, 3, 4))(x => x % 2 == 0) ==== List(2, 4)
    }
  }

  "exercises 3.22 - add list" >> {
    "adds the elements of two list" in {
      List.addLists(List(1, 2, 3), List(4, 5, 6)) ==== List(5, 7, 9)
    }
  }

  "exercises 3.23 - zipWith" >> {
    "zips two list using the given function" in {
      List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) ==== List(5, 7, 9)
    }
  }

  "exercises 3.24 - hasSubsequence" >> {
    "returns true when a list is contained in another" in {
      List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) ==== true
    }

    "returns true when both sequences are nil" in {
      List.hasSubsequence(Nil, Nil) ==== true
    }

    "returns true when second sequence is nil" in {
      List.hasSubsequence(List(1, 2, 3), Nil) ==== true
    }

    "returns true when both sequences are equal" in {
      List.hasSubsequence(List(1, 2), List(1, 2)) ==== true
    }

    "returns false when first list is nil and second is not" in {
      List.hasSubsequence(Nil: List[Int], List(1)) ==== false
    }
  }
}
