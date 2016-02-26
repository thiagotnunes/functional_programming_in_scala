package com.functionalprogramming.chapter10

import org.specs2.mutable.Specification

class MonoidSpec extends Specification {
  "exercises 10.5" >> {
    "foldMap" >> {
      "maps and folds list" in {
        Monoid.foldMap(List(1, 2, 3), Monoid.stringMonoid)(_.toString) ==== "123"
      }
    }
  }

  "exercises 10.6" >> {
    "foldLeft" >> {
      "returns folded list" in {
        Monoid.foldLeft(List(1, 2, 3))(List.empty[Int])((tail, head) => head +: tail) ==== List(3, 2, 1)
      }
    }

    "foldRight" >> {
      "returns folded list" in {
        Monoid.foldRight(List(1, 2, 3))(List.empty[Int])((head, tail) => head +: tail) ==== List(1, 2, 3)
      }
    }
  }

  "exercises 10.7" >> {
    "foldMapV" >> {
      "maps and folds list" in {
        Monoid.foldMapV("1234", Monoid.intAddition)(_.asDigit) ==== 10
      }
    }
  }

  "exercises 10.9" >> {
    "is sorted" >> {
      // Does not work for foldMapV, I think this is wrong, because it breaks the associativity monoid law
      val monoid = new Monoid[Option[Int]] {
        override def op(a1: Option[Int], a2: Option[Int]): Option[Int] = {
          (a1, a2) match {
            case (Some(v1), Some(v2)) if v1 <= v2 => a2
            case _ => None
          }
        }

        override def zero: Option[Int] = Some(Integer.MIN_VALUE)
      }

      "returns true when seq is sorted" in {
        Monoid.foldMap(List(1, 2, 3, 4), monoid)(Some(_)).exists(_ => true) ==== true
      }

      "returns false when seq is not sorted" in {
        Monoid.foldMap(List(1, 3, 2, 4), monoid)(Some(_)).exists(_ => true) ==== false
      }
    }
  }

  "exercises 10.16" >> {
    "product monoid" >> {
      val productMonoid = Monoid.productMonoid(Monoid.intAddition, Monoid.intMultiplication)

      "returns respective" in {
        productMonoid.zero ==== (0, 1)
      }

      "respects zero law" in {
        productMonoid.op(productMonoid.zero, (3, 4)) ==== (3, 4)
      }

      "respects associative law" in {
        productMonoid.op((5, 10), (3, 4)) ==== productMonoid.op((3, 4), (5, 10))
      }
    }
  }

  "exercises 10.18" >> {
    "bag" >> {
      "returns bag from indexed seq" in {
        Monoid.bag(Array("a", "rose", "is", "a", "rose")) ==== Map("a" -> 2, "rose" -> 2, "is" -> 1)
      }
    }
  }
}
