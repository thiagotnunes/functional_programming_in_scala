package com.functionalprogramming.chapter8

import com.functionalprogramming.chapter6.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {
  def toOption: Gen[Option[A]] = {
    Gen(sample.map(Some(_)))
  }

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(a => f(a)))
  }

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
    Gen(sample.map2(g.sample)(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => Gen.listOfN(n, this))
  }

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] = {
    this.map2(g)((_, _))
  }
}

object Gen {
  private def nextInt(rng: RNG, start: Int, stop: Int): (Int, RNG) = {
    val (a, newRng) = rng.nextInt
    if (a >= 0) {
      (((a / Int.MaxValue.toDouble) * (stop - start) + start).toInt, newRng)
    } else {
      (((a / Int.MinValue.toDouble) * (stop - start) + start).toInt, newRng)
    }
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State[RNG, Int](rng => nextInt(rng, start, stopExclusive - 1)))
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State(rng => (a, rng)))
  }

  def boolean: Gen[Boolean] = {
    Gen(State[RNG, Int](rng => rng.nextInt).map(_ >= 0))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(0.until(n).map(_ => g.sample).toList))
  }

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    Gen(for {
      a <- choose(start, stopExclusive).sample
      b <- choose(start, stopExclusive).sample
    } yield {
      (a, b)
    })
  }

  def stringOfNWithInt(n: Int, g: Gen[Int]): Gen[String] = {
    Gen(listOfN(n, g).sample.map(ints => ints.map(_.toChar).mkString))
  }

  def stringOfN(n: Int, g: Gen[Char]): Gen[String] = {
    Gen(listOfN(n, g).sample.map(_.mkString))
  }

  def numeric: Gen[Char] = {
    Gen(choose(48, 58).sample.map(_.toChar))
  }

  def lowerCaseAlpha: Gen[Char] = {
    Gen(choose(97, 123).sample.map(_.toChar))
  }

  def upperCaseAlpha: Gen[Char] = {
    Gen(choose(65, 91).sample.map(_.toChar))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    Gen(boolean.sample.flatMap(bool => if (bool) g1.sample else g2.sample))
  }

  def alphaNumeric: Gen[Char] = {
    union(union(lowerCaseAlpha, upperCaseAlpha), numeric)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val total = g1._2 + g2._2
    val weight1 = ((g1._2 / total) * 100).toInt
    val weight2 = ((g2._2 / total) * 100).toInt

    Gen(choose(0, weight1 + weight2).sample.flatMap(i => if (i < weight1) g1._1.sample else g2._1.sample))
  }
}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}
