package com.functionalprogramming.chapter12

import com.functionalprogramming.chapter11.Monad
import com.functionalprogramming.chapter4.{Either, Left, Right}

object Chapter12 {
  val streamApplicative = new ApplicativeMap2[Stream] {
    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] = {
      fa.zip(fb).map(f.tupled)
    }

    override def unit[A](a: => A): Stream[A] = {
      Stream.continually(a)
    }

    // Exercise 12.4
    // Makes the list of streams streamable
  }

  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = {
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = {
        Right(a)
      }

      override def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]): Either[E, B] = {
        ma match {
          case Left(e) => Left(e)
          case Right(a) => f(a)
        }
      }
    }
  }
}
