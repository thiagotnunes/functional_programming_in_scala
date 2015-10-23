package com.functionalprogramming.chapter3

import org.specs2.mutable.Specification

class TreesSpec extends Specification {

  val trees = new Trees

  "exercise 3.25 - size" >> {
    "returns the size of the tree" in {
      trees.size(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))) ==== 7
    }

    "returns 1 for a leaf" in {
      trees.size(Leaf(2)) ==== 1
    }
  }

  "exercise 3.26 - maximum" in {
    "returns the max element of the tree" in {
      trees.maximum(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(4))), Leaf(3))) ==== 4
    }

    "returns the max element when all elements are the same" in {
      trees.maximum(Branch(Branch(Leaf(1), Branch(Leaf(1), Leaf(1))), Leaf(1))) ==== 1
    }
  }

  "exercise 3.27 - depth" in {
    "returns the maximum depth of the tree" in {
      trees.depth(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))) ==== 4
    }
  }

  "exercise 3.28 - map" in {
    "maps each element of the tree" in {
      val result = trees.map(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)))(x => x + 1)
      result ==== Branch(Branch(Leaf(2), Branch(Leaf(3), Leaf(4))), Leaf(5))
    }
  }

  "exercise 3.29 - fold" in {
    "returns the size of the tree" in {
      trees.sizeViaFold(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))) ==== 7
    }

    "returns the max element of the tree" in {
      trees.maximumViaFold(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(4))), Leaf(3))) ==== 4
    }

    "returns the maximum depth of the tree" in {
      trees.depthViaFold(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))) ==== 4
    }

    "maps each element of the tree" in {
      val result = trees.mapViaFold(Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4)))(x => x + 1)
      result ==== Branch(Branch(Leaf(2), Branch(Leaf(3), Leaf(4))), Leaf(5))
    }
  }
}
