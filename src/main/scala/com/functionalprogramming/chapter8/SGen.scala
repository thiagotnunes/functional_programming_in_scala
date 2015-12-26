package com.functionalprogramming.chapter8

case class SGen[A](forSize: Int => Gen[A]) {
  def toOption: SGen[Option[A]] = {
    SGen(n => forSize(n).toOption)
  }
}

object SGen {
  def choose(start: Int, stopExclusive: Int): SGen[Int] = {
    SGen(_ => Gen.choose(start, stopExclusive))
  }

  def unit[A](a: => A): SGen[A] = {
    SGen(_ => Gen.unit(a))
  }

  def boolean: SGen[Boolean] = {
    SGen(_ => Gen.boolean)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => Gen.listOfN(n, g))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => Gen.listOfN(n + 1, g))
  }
}
