package com.functionalprogramming.chapter4

import org.specs2.mutable.Specification

class OptionSpec extends Specification {

  "exercises 4.1" >> {
    "map" >> {
      "returns the mapped value for Some" in {
        Some(1).map(_ + 1) ==== Some(2)
      }

      "returns None for None" in {
        None.map((i: Int) => i + 1) ==== None
      }
    }

    "flatMap" >> {
      "returns flatMapped value for Some" in {
        Some(1).flatMap(i => Some(i + 2)) ==== Some(3)
      }

      "returns None for Some when mapping function returns None" in {
        Some(1).flatMap(_ => None) ==== None
      }

      "returns None for None" in {
        None.flatMap((i: Int) => Some(i + 1)) ==== None
      }
    }

    "getOrElse" >> {
      "returns value wrapped by Some for Some" in {
        Some(1).getOrElse(-1) ==== 1
      }

      "returns default value for None" in {
        None.getOrElse(-1) ==== -1
      }
    }

    "orElse" >> {
      "returns itself for Some" in {
        Some(1).orElse(None) ==== Some(1)
      }

      "returns else value for None" in {
        None.orElse(Some(1)) ==== Some(1)
      }
    }

    "filter" >> {
      "returns itself when the predicate is true for Some" in {
        Some(2).filter(_ % 2 == 0) ==== Some(2)
      }

      "returns None when the predicate is false for None" in {
        Some(1).filter(_ % 2 == 0) ==== None
      }

      "returns None for None" in {
        None.filter((i: Int) => i % 2 == 0) ==== None
      }
    }
  }

  "exercises 4.3" >> {
    "returns Some mapped value when both options are defined" in {
      Option.map2(Some(1), Some(2))(_ + _) ==== Some(3)
    }

    "returns None when first option is not defined" in {
      Option.map2(None, Some(1))((a: Int, b: Int) => a + b) ==== None
    }

    "returns None when second option is not defined" in {
      Option.map2(Some(1), None)((a: Int, b: Int) => a + b) ==== None
    }

    "returns None when both option is not defined" in {
      Option.map2(None, None)((a: Int, b: Int) => a + b) ==== None
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
