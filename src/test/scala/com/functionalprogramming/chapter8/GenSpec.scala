package com.functionalprogramming.chapter8

import com.functionalprogramming.chapter6.{IdentityRNG, SimpleRNG}
import org.specs2.mutable.Specification

class GenSpec extends Specification {

  "exercises 8.4" >> {
    "choose" >> {
      "returns predecessor of stop param when rng returns int max value" in {
        val rng = new IdentityRNG(Int.MaxValue)
        Gen.choose(1, 10).sample.run(rng)._1 ==== 9
      }

      "returns predecessor of stop param when rng returns int min value" in {
        val rng = new IdentityRNG(Int.MinValue)
        Gen.choose(1, 10).sample.run(rng)._1 ==== 9
      }

      "returns start param when rng returns 0" in {
        val rng = new IdentityRNG(0)
        Gen.choose(1, 10).sample.run(rng)._1 ==== 1
      }

      "returns middle when rng returns middle" in {
        val rng = new IdentityRNG(Int.MaxValue / 2 + 1)
        Gen.choose(1, 10).sample.run(rng)._1 ==== 5
      }
    }
  }

  "exercises 8.5" >> {
    "unit" >> {
      "returns the given value" in {
        Gen.unit("a").sample.run(new SimpleRNG(10))._1 ==== "a"
      }
    }

    "boolean" >> {
      "returns false when rng returns negative number" >> {
        val rng = new IdentityRNG(-1)
        Gen.boolean.sample.run(rng)._1 ==== false
      }

      "returns true when rng returns 0" >> {
        val rng = new IdentityRNG(0)
        Gen.boolean.sample.run(rng)._1 ==== true
      }

      "returns true when rng returns positive number" >> {
        val rng = new IdentityRNG(1)
        Gen.boolean.sample.run(rng)._1 ==== true
      }
    }

    "listOfN" >> {
      "returns a list using the given generator" >> {
        Gen.listOfN(5, Gen.choose(1, 10)).sample.run(new SimpleRNG(10))._1 ==== List(1, 5, 6, 3, 6)
      }
    }
  }

  "optional exercises" >> {
    "choosePair" >> {
      "returns the a pair of ints between the given range" in {
        Gen.choosePair(1, 10).sample.run(new SimpleRNG(10))._1 ====(1, 5)
      }
    }

    "toOption" >> {
      "returns an Gen[Option[A]] from a Gen[A]" in {
        Gen.choose(1, 10).toOption.sample.run(new SimpleRNG(10))._1 ==== Some(1)
      }
    }

    // Gen[A] from Gen[Option[A]] is not possible, because of the None

    "stringOfNWithInt" >> {
      "returns a string from a Gen[Int]" in {
        Gen.stringOfNWithInt(5, Gen.choose(48, 91)).sample.run(new SimpleRNG(10))._1 ==== "0JM=L"
      }
    }

    "stringOfN" >> {
      "returns a string from a Gen[Char]" in {
        Gen.stringOfN(5, Gen.lowerCaseAlpha).sample.run(new SimpleRNG(10))._1 ==== "apriq"
      }
    }

    "alphaNumeric" >> {
      "returns lower case letters" in {
        Gen.alphaNumeric.sample.run(new SimpleRNG(10))._1 ==== 'r'
      }

      "returns upper case letters" in {
        Gen.alphaNumeric.sample.run(new SimpleRNG(1000))._1 ==== 'Y'
      }

      "returns numbers" in {
        Gen.alphaNumeric.sample.run(new SimpleRNG(10000))._1 ==== '5'
      }
    }
  }

  "exercises 8.6" >> {
    "flatMap" >> {
      "transforms Gen[A] to Gen[B]" in {
        Gen.choose(1, 10).flatMap(i => Gen.unit(i)).sample.run(IdentityRNG(0))._1 ==== 1
      }
    }

    "listOfN" >> {
      "returns a generator of size defined by the given generator of int" in {
        Gen.unit('a').listOfN(Gen.choose(1, 5)).sample.run(IdentityRNG(Int.MaxValue))._1 ==== List('a', 'a', 'a', 'a')
      }
    }
  }

  "exercises 8.7" >> {
    "union" >> {
      "returns a generator that is the union of both given" in {
        Gen.union(Gen.lowerCaseAlpha, Gen.upperCaseAlpha).sample.run(SimpleRNG(10))._1 ==== 'p'
      }
    }
  }

  "exercises 8.8" >> {
    "weighted" >> {
      "generates values according to the given weights" in {
        Gen.weighted((Gen.lowerCaseAlpha, 1), (Gen.upperCaseAlpha, 100)).sample.run(SimpleRNG(10))._1 ==== 'P'
      }
    }
  }
}
