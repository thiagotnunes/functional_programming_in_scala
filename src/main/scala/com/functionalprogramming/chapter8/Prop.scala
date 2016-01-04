package com.functionalprogramming.chapter8

import java.util.concurrent.Executors

import com.functionalprogramming.chapter5.Stream
import com.functionalprogramming.chapter6.{RNG, SimpleRNG}
import com.functionalprogramming.chapter7.Par

case class Prop(tag: Tag, run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = {
    Prop(tag + p.tag, {
      (max, n, rng) => run(max, n, rng) match {
        // this is ugly but we can not run both at the same time due to non-strictness of &&
        case Passed => p.run(max, n, rng) match {
          case Passed => Passed
          case falsified@Falsified(t, f, s) => falsified
        }
        case falsified@Falsified(t, f, s) => falsified
      }
    })
  }

  def ||(p: Prop): Prop = {
    Prop(tag + p.tag, {
      // this is ugly but we can not run both at the same time due to non-strictness of ||
      (max, n, rng) => run(max, n, rng) match {
        case Passed => Passed
        case Falsified(_, _, _) => p.run(max, n, rng) match {
          case Passed => Passed
          case falsified@Falsified(_, _, _) => falsified
        }
      }
    })
  }
}

object Prop {
  private val S = Gen.weighted(
    Gen.choose(1, 4).map(n => Executors.newFixedThreadPool(n)) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )

  def forAllPar[A](tag: Tag, g: Gen[A])(f: A => Par[Boolean]): Prop = {
    forAll(tag, S ** g) { case s ** a => f(a)(s).get }
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = {
    Par.map2(p, p2)(_ == _)
  }

  def checkPar(p: => Par[Boolean]): Prop = Prop("", { (_, _, rng) =>
    val p1 = Par.run(S.sample.run(rng)._1)(p).get()
    if (p1)
      Proved
    else
      Falsified("", "()", 0)
  })

  def check(p: => Boolean): Prop = Prop("", { (_, _, _) =>
    if (p)
      Proved
    else
      Falsified("", "()", 0)
  })

  def forAll[A](tag: Tag, as: Gen[A])(f: A => Boolean): Prop = Prop(tag, {
    (_, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(tag, a.toString, i)
      } catch {
        case e: Exception => Falsified(tag, buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  })

  def forAll[A](tag: Tag, g: SGen[A])(f: A => Boolean): Prop = forAll(tag, g.forSize)(f)

  def forAll[A](tag: Tag, g: Int => Gen[A])(f: A => Boolean): Prop = Prop(tag, {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(tag, g(i))(f))
      val prop: Prop = props.map(p => Prop(tag, { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      })).toList.reduce(_ && _)
      prop.run(max, n, rng)
  })

  def run(p: Prop,
          maxSize: MaxSize = 100,
          testCases: TestCases,
          rng: RNG = new SimpleRNG(System.currentTimeMillis())): Unit = {
    p.run(maxSize, testCases, rng) match {
      case Falsified(tag, msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
  }

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  private def buildMsg[A](s: A, e: Exception): String = {
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }
}
