package com.functionalprogramming.chapter8

import com.functionalprogramming.chapter6.SimpleRNG
import org.specs2.mutable.Specification

class PropSpec extends Specification {

  "exercises 8.9" >> {
    "forAll" >> {
      "returns passed when all the test cases pass" in {
        Prop.forAll("alphaNumeric")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l.matches("[a-zA-Z0-9]+"))
          .run(10, new SimpleRNG(20)) ==== Passed
      }

      "returns falsified when at least one of the test cases fail" in {
        Prop.forAll("alphaNumeric")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l.matches("[A-Z0-9]+"))
          .run(10, new SimpleRNG(20)) ==== Falsified("alphaNumeric", "7dW", 2)
      }
    }

    "&&" >> {
      "returns passed when both properties are true" in {
        val p1 = Prop.forAll("identity")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l == l)
        val p2 = Prop.forAll("length")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l.length == l.reverse.length)

        (p1 && p2).run(10, SimpleRNG(10)) ==== Passed
      }

      "returns falsified when first property is false" in {
        val p1 = Prop.forAll("false")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l != l)
        val p2 = Prop.forAll("length")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l.length == l.reverse.length)

        (p1 && p2).run(10, SimpleRNG(10)) ==== Falsified("false", "rJi", 0)
      }

      "returns falsified when second property is false" in {
        val p1 = Prop.forAll("length")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l.length == l.reverse.length)
        val p2 = Prop.forAll("false")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l != l)

        (p1 && p2).run(10, SimpleRNG(10)) ==== Falsified("false", "rJi", 0)
      }

      "returns falsified when both properties are false" in {
        val p1 = Prop.forAll("false1")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l != l)
        val p2 = Prop.forAll("false2")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l.length != l.reverse.length)

        (p1 && p2).run(10, SimpleRNG(10)) ==== Falsified("false1", "rJi", 0)
      }
    }

    "||" >> {
      "returns passed when both properties are true" in {
        val p1 = Prop.forAll("identity")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l == l)
        val p2 = Prop.forAll("length")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l.length == l.reverse.length)

        (p1 || p2).run(10, SimpleRNG(10)) ==== Passed
      }

      "returns passed when first property is true" in {
        val p1 = Prop.forAll("identity")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l == l)
        val p2 = Prop.forAll("false")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l.length != l.reverse.length)

        (p1 || p2).run(10, SimpleRNG(10)) ==== Passed
      }

      "returns passed when second property is true" in {
        val p1 = Prop.forAll("identity")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l == l)
        val p2 = Prop.forAll("false")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l.length != l.reverse.length)

        (p2 || p1).run(10, SimpleRNG(10)) ==== Passed
      }

      "returns falsified when both properties are false" in {
        val p1 = Prop.forAll("false1")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l != l)
        val p2 = Prop.forAll("false2")(Gen.stringOfN(3, Gen.alphaNumeric))(l => l.length != l.reverse.length)

        (p1 || p2).run(10, SimpleRNG(10)) ==== Falsified("false2", "rJi", 0)
      }
    }
  }

}
