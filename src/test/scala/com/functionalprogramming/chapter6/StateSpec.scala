package com.functionalprogramming.chapter6

import org.specs2.mutable.Specification

class StateSpec extends Specification {
  val rand = State[RNG, Int](rng => rng.nextInt)

  "exercise 6.10" >> {
    "unit" >> {
      "returns unit state" in {
        State.unit(1).run(SimpleRNG(1)) ==== (1, SimpleRNG(1))
      }
    }

    "map" >> {
      "maps over value for the given state" in {
        rand.map(_ + 1).run(IdentityRNG(1)) ==== (2, IdentityRNG(1))
      }
    }

    "map2" >> {
      "combines two values for the given states" in {
        rand.map2(rand)((a, b) => a + b).run(SimpleRNG(3)) ==== (833900052, SimpleRNG(54574829143541L))
      }
    }

    "flatMap" >> {
      "flatMaps over the state" in {
        rand.flatMap(a => State(s => (a + 1, s))).run(IdentityRNG(1)) ==== (2, IdentityRNG(1))
      }
    }

    "sequence" >> {
      "returns state of list" in {
        State.sequence(List(
          State[RNG, Int](s => (s.nextInt._1 + 1, s.nextInt._2)),
          State[RNG, Int](s => (s.nextInt._1 + 2, s.nextInt._2)),
          State[RNG, Int](s => (s.nextInt._1 + 3, s.nextInt._2))
        )).run(IdentityRNG(0)) ==== (List(1,2,3), IdentityRNG(0))
      }
    }
  }
}
