package com.functionalprogramming.chapter6

import com.functionalprogramming.chapter6
import org.specs2.mutable.Specification

class RNGExercisesSpec extends Specification {
  private val exercises = new RNGExercises

  "exercises 6.1" >> {
    "nonNegativeInt" >> {
      "returns non negative int when rng returns positive value" in {
        exercises.nonNegativeInt(SimpleRNG(234))._1 ==== 90031242
      }

      "returns non negative int when rng returns negative value" in {
        exercises.nonNegativeInt(SimpleRNG(-21321890))._1 ==== 186190209
      }

      "handles the case of Int.MinValue" in {
        exercises.nonNegativeInt(IdentityRNG(Int.MinValue))._1 ==== Int.MaxValue
      }
    }
  }

  "exercises 6.2" >> {
    "double" >> {
      "returns a 0 when RNG returns 0" in {
        exercises.double(IdentityRNG(0))._1 ==== 0
      }

      "returns a 0.9999 when RNG returns int max value" in {
        val nextDouble = exercises.double(IdentityRNG(Int.MaxValue))._1
        nextDouble !=== 1.0
        nextDouble must beCloseTo(0.9999, 0.0001)
      }

      "returns a 0.9999 when RNG returns int min value" in {
        val nextDouble = exercises.double(IdentityRNG(Int.MinValue))._1
        nextDouble !=== 1.0
        nextDouble must beCloseTo(0.9999, 0.0001)
      }
    }
  }

  "exercises 6.3" >> {
    "intDouble" >> {
      "returns a random int and random a double" in {
        val ((nextInt, nextDouble), _) = exercises.intDouble(IdentityRNG(Int.MaxValue))
        nextInt ==== Int.MaxValue
        nextDouble must beCloseTo(0.9999, 0.0001)
      }
    }

    "doubleInt" >> {
      "returns a random double and a random int" in {
        val ((nextDouble, nextInt), _) = exercises.doubleInt(SimpleRNG(456))
        nextInt ==== 1384243008
        nextDouble must beCloseTo(0.0816, 0.0001)
      }
    }

    "double3" >> {
      "returns 3 random doubles" in {
        val (doubles, _) = exercises.double3(SimpleRNG(10))
        doubles._1 must beCloseTo(0.0017, 0.0001)
        doubles._2 must beCloseTo(0.6213, 0.0001)
        doubles._3 must beCloseTo(0.6923, 0.0001)
      }
    }
  }

  "exercises 6.4" >> {
    "ints" >> {
      "returns a list of random integers" in {
        exercises.ints(5)(SimpleRNG(987))._1 ==== List(379747164, 2002256502, -1282855726, 2131553239, 2066082197, 1545837154)
      }
    }
  }

  "exercises 6.5" >> {
    "doubleViaMap" >> {
      "returns a 0 when RNG returns 0" in {
        exercises.doubleViaMap(IdentityRNG(0))._1 ==== 0
      }

      "returns a 0.9999 when RNG returns int max value" in {
        val nextDouble = exercises.doubleViaMap(IdentityRNG(Int.MaxValue))._1
        nextDouble !=== 1.0
        nextDouble must beCloseTo(0.9999, 0.0001)
      }

      "returns a 0.9999 when RNG returns int min value" in {
        val nextDouble = exercises.doubleViaMap(IdentityRNG(Int.MinValue))._1
        nextDouble !=== 1.0
        nextDouble must beCloseTo(0.9999, 0.0001)
      }
    }
  }

  "exercises 6.6" >> {
    "randIntDouble" >> {
      "returns a random int and random a double" in {
        val ((nextInt, nextDouble), _) = exercises.randIntDouble(IdentityRNG(Int.MaxValue))
        nextInt ==== Int.MaxValue
        nextDouble must beCloseTo(0.9999, 0.0001)
      }
    }

    "randDoubleInt" >> {
      "returns a random double and a random int" in {
        val ((nextDouble, nextInt), _) = exercises.randDoubleInt(SimpleRNG(456))
        nextInt ==== 1384243008
        nextDouble must beCloseTo(0.0816, 0.0001)
      }
    }
  }

  "exercises 6.7" >> {
    "sequence" >> {
      "returns a sequence of random ints" in {
        sequence(List(exercises.int, exercises.int, exercises.int))(SimpleRNG(123))._1 ==== List(
          47324114,
          -386449838,
          806037626
        )
      }
    }
  }

  "exercises 6.8" >> {
    "nonNegativeLess" >> {
      "returns non negative int less than the given value" in {
        exercises.nonNegativeLessThan(10)(SimpleRNG(123))._1 ==== 4
      }
    }
  }

  "exercises 6.9" >> {
    "mapViaFlatMap" in {
      "returns mapped value" in {
        chapter6.mapViaFlatMap(exercises.int)(i => i + 1)(IdentityRNG(1))._1 ==== 2
      }
    }

    "map2ViaFlatMap" in {
      "returns mapped values" in {
        chapter6.map2ViaFlatMap(exercises.int, exercises.int)(_ + _)(IdentityRNG(2))._1 ==== 4
      }
    }
  }
}
