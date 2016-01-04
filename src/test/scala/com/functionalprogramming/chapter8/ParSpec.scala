package com.functionalprogramming.chapter8

import com.functionalprogramming.chapter6.SimpleRNG
import com.functionalprogramming.chapter7.Par
import org.specs2.mutable.Specification

class ParSpec extends Specification {

  "extra" >> {
    "checks mapping" in {
      Prop.checkPar {
        Prop.equal(
          Par.map(Par.unit(1))(_ + 1),
          Par.unit(2)
        )
      }.run(1, 1, new SimpleRNG(100)) ==== Proved
    }

    "checks identity" in {
      val pint = Gen.choose(0, 10).map(Par.unit)
      Prop.forAllPar("", pint)(n => Prop.equal(Par.map(n)(y => y), n))
        .run(50, 50, new SimpleRNG(200)) ==== Passed
    }
  }

  "exercises 8.16" in {
    "not implemented yet" == null
  }.pendingUntilFixed

  "exercises 8.17" >> {
    "fork property" >> {
      "returns passed" in {
        val pint = Gen.choose(0, 10).map(Par.unit)
        Prop.forAllPar("", pint)(n => Prop.equal(Par.fork(n), n))
          .run(100, 100, new SimpleRNG(300)) ==== Passed
      }
    }
  }

  // "exercises 8.18" >> {
    // takeWhile on empty list returns the empty list
    // takeWhile on a function that returns true returns the whole list
    // takeWhile on a function that returns false returns the empty list
    // a function f when applied to takeWhile returns the opposite of when applied to dropWhile
  //}

}
