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
}
