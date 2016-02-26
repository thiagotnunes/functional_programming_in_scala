package com.functionalprogramming.chapter10

import com.functionalprogramming.chapter3.{Leaf, Branch}
import org.specs2.mutable.Specification
import com.functionalprogramming.chapter5.Stream

class FoldableSpec extends Specification {

  "exercises 10.12" >> {
    "foldable list" >> {
      "folds list left" in {
        Foldable.listFoldable.foldLeft(List(1, 2, 3))(List.empty[Int])((acc, e) => e +: acc) ==== List(3, 2, 1)
      }

      "folds list right" in {
        Foldable.listFoldable.foldRight(List(1, 2, 3))(List.empty[Int])((e, acc) => e +: acc) ==== List(1, 2, 3)
      }

      "folds and maps list" in {
        Foldable.listFoldable.foldMap(List(1, 2, 3))(_.toString)(Monoid.stringMonoid) ==== "123"
      }
    }

    "foldable indexed seq" >> {
      "folds seq left" in {
        Foldable.indexedSeqFoldable.foldLeft(Array(1, 2, 3))(Array.empty[Int])((acc, e) => e +: acc) ==== Array(3, 2, 1)
      }

      "folds seq right" in {
        Foldable.indexedSeqFoldable.foldRight(Array(1, 2, 3))(Array.empty[Int])((e, acc) => e +: acc) ==== Array(1, 2, 3)
      }

      "folds and maps seq" in {
        Foldable.indexedSeqFoldable.foldMap(Array(1, 2, 3))(_.toString)(Monoid.stringMonoid) ==== "123"
      }
    }

    "foldable stream" >> {
      "folds stream left" in {
        Foldable.streamFoldable.foldLeft(Stream(1, 2, 3))(Stream.empty[Int])((acc, e) => Stream.cons(e, acc)).toList ==== Stream(3, 2, 1).toList
      }

      "folds stream right" in {
        Foldable.streamFoldable.foldRight(Stream(1, 2, 3))(Stream.empty[Int])((e, acc) => Stream.cons(e, acc)).toList ==== Stream(1, 2, 3).toList
      }

      "folds and maps stream" in {
        Foldable.streamFoldable.foldMap(Stream(1, 2, 3))(_.toString)(Monoid.stringMonoid) ==== "123"
      }
    }
  }

  "exercises 10.13" >> {
    "foldable tree" in {
      "folds tree left" in {
        Foldable.treeFoldable.foldLeft(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(List.empty[Int])((acc, e) => e +: acc) ==== List(3, 2, 1)
      }

      "folds tree right" in {
        Foldable.treeFoldable.foldRight(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(List.empty[Int])((e, acc) => e +: acc) ==== List(1, 2, 3)
      }

      "folds and maps tree" in {
        Foldable.treeFoldable.foldMap(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_.toString)(Monoid.stringMonoid) ==== "123"
      }
    }
  }

  "exercises 10.14" >> {
    "foldable option" in {
      "folds option left" in {
        Foldable.optionFoldable.foldLeft(Some(1))("")((acc, e) => acc + e) ==== "1"
        Foldable.optionFoldable.foldLeft(None)("")((acc, e) => acc + e) ==== ""
      }

      "folds option right" in {
        Foldable.optionFoldable.foldRight(Some(1))("")((acc, e) => acc + e) ==== "1"
        Foldable.optionFoldable.foldRight(None)("")((acc: String, e: String) => acc + e) ==== ""
      }

      "folds and maps option" in {
        Foldable.optionFoldable.foldMap(Some(1))(_.toString)(Monoid.stringMonoid) ==== "1"
        Foldable.optionFoldable.foldMap(None)(_.toString)(Monoid.stringMonoid) ==== ""
      }
    }
  }

  "exercises 10.15" >> {
    "toList" >> {
      "returns list from list" in {
        Foldable.listFoldable.toList(List(1, 2, 3)) ==== List(1, 2, 3)
      }

      "returns list from indexed seq" in {
        Foldable.indexedSeqFoldable.toList(Array(1, 2, 3)) ==== List(1, 2, 3)
      }

      "returns list from stream" in {
        Foldable.streamFoldable.toList(Stream(1, 2, 3)) ==== List(1, 2, 3)
      }

      "returns list from tree" in {
        Foldable.treeFoldable.toList(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) ==== List(1, 2, 3)
      }

      "returns list from option" in {
        Foldable.optionFoldable.toList(Some(1)) ==== List(1)
        Foldable.optionFoldable.toList(None) ==== List()
      }
    }
  }

}
