package com.functionalprogramming.chapter8

import com.functionalprogramming.chapter6.SimpleRNG
import org.specs2.mutable.Specification

class SGenSpec extends Specification {

  "exercises 8.12" >> {
    "listOf" >> {
      "generates list of the given size" in {
        SGen.listOf(Gen.alphaNumeric).forSize(10).sample.run(SimpleRNG(10))._1.length ==== 10
      }
    }
  }

  "exercises 8.13" >> {
    "max" >> {
      "asserts there is nothing greater than the max of a list" in {
        val smallInt = Gen.choose(-10, 10)
        val maxProp = Prop.forAll("max", SGen.listOf1(smallInt)) { ns =>
          val max = ns.max
          !ns.exists(_ > max)
        }
        maxProp.run(100, 100, SimpleRNG(System.currentTimeMillis())) ==== Passed
      }
    }
  }

  "exercises 8.14" >> {
    "sorted" >> {
      "asserts that in a sorted list the elements are in ascending order" in {
        val listElementsGen = Gen.choose(-100, 100)
        val ascendingProp = Prop.forAll("ascending", SGen.listOf(listElementsGen)) { ns =>
          val sorted = ns.sorted
          val pairs = sorted.zip(sorted.drop(1))
          pairs.forall { case (a, b) => a <= b }
        }
        val maxProp = Prop.forAll("max", SGen.listOf1(listElementsGen)) { ns =>
          val sorted = ns.sorted
          val max = ns.max

          sorted.last == max
        }
        val sortedProp = ascendingProp && maxProp

        sortedProp.run(100, 100, SimpleRNG(System.currentTimeMillis())) ==== Passed
      }
    }
  }

}
