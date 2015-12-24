package com.functionalprogramming.chapter8

import com.functionalprogramming.chapter6.RNG
import com.functionalprogramming.chapter5.Stream
import com.functionalprogramming.chapter8.Prop.{Tag, SuccessCount, FailedCase, TestCases}

case class Prop(tag: Tag, run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = {
    Prop(tag + p.tag, {
      (n, rng) => run(n, rng) match {
        // this is ugly but we can not run both at the same time due to non-strictness of &&
        case Passed => p.run(n, rng) match {
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
      (n, rng) => run(n, rng) match {
        case Passed => Passed
        case Falsified(_, _, _) => p.run(n, rng) match {
          case Passed => Passed
          case falsified@Falsified(_, _, _) => falsified
        }
      }
    })
  }
}

object Prop {
  type Tag = String
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](tag: Tag)(as: Gen[A])(f: A => Boolean): Prop = Prop(tag, {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(tag, a.toString, i)
      } catch {
        case e: Exception => Falsified(tag, buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  })

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  private def buildMsg[A](s: A, e: Exception): String = {
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(tag: Tag, failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}
