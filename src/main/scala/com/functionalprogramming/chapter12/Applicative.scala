package com.functionalprogramming.chapter12

import com.functionalprogramming.chapter11.{Functor, Monad}
import com.functionalprogramming.chapter4.{Either, Left, None, Option, Right, Some}

trait ApplicativeMap2[F[_]] extends Functor[F] {
  self =>
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =

  def unit[A](a: => A): F[A]

  // derived combinators
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    map2(fa, unit(()))((a, _) => f(a))
  }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = {
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
  }

  // Exercise 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    traverse(fas)(fa => fa)
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    map2(fa, unit(()))((a, _) => List.fill(n)(a))
  }

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    map2(fa, fb)((a, b) => (a, b))
  }

  // Exercise 12.8
  def product[G[_]](G: ApplicativeMap2[G]): ApplicativeMap2[({type f[x] = (F[x], G[x])})#f] = {
    new ApplicativeMap2[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = {
        (self.unit(a), G.unit(a))
      }

      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
        (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }
    }
  }

  // Exercise 12.9
  def compose[G[_]](G: ApplicativeMap2[G]): ApplicativeMap2[({type f[x] = F[G[x]]})#f] = {
    new ApplicativeMap2[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = {
        self.unit(G.unit(a))
      }

      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        self.map2(fa, fb)(G.map2(_, _)(f))
      }
    }
  }

  // Exercise 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    ofa.foldRight(unit(Map.empty[K, V]))((entry, facc) => {
      map2(entry._2, facc)((value, acc) => acc + ((entry._1, value)))
    })
  }

  // Exercise 12.18
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (G: Applicative[G], H: Applicative[H]):
                            (G[F[B]], H[F[B]]) = {
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)
  }
}

trait ApplicativeApply[F[_]] extends Functor[F] {
  self =>
  //primitive combinators
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def unit[A](a: => A): F[A]

  // Exercise 12.2
  def map[A, B](fa: F[A])(f: A => B): F[B] = {
    apply(unit(f): F[A => B])(fa)
  }

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(apply(unit(f.curried): F[A => B => C])(fa))(fb)
  }

  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(
      apply(
        apply(unit(f.curried): F[A => B => C => D])(fa)
      )(fb)
    )(fc)
  }

  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(
      apply(
        apply(
          apply(unit(f.curried): F[A => B => C => D => E])(fa)
        )(fb)
      )(fc)
    )(fd)
  }

  // Exercise 12.8
  def product[G[_]](G: ApplicativeApply[G]): ApplicativeApply[({type f[x] = (F[x], G[x])})#f] = {
    new ApplicativeApply[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = {
        (self.unit(a), G.unit(a))
      }

      override def apply[A, B](fab: (F[(A) => B], G[(A) => B]))(fa: (F[A], G[A])): (F[B], G[B]) = {
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
      }
    }
  }
}

object Applicative {
  implicit val optionApplicative: ApplicativeMap2[Option] = new ApplicativeMap2[Option] {
    override def unit[A](a: => A): Option[A] = {
      Some(a)
    }

    override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = {
      (fa, fb) match {
        case (Some(a), Some(b)) => Some(f(a, b))
        case _ => None
      }
    }
  }

  implicit val streamApplicative: ApplicativeMap2[Stream] = new ApplicativeMap2[Stream] {
    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] = {
      fa.zip(fb).map(f.tupled)
    }

    override def unit[A](a: => A): Stream[A] = {
      Stream.continually(a)
    }

    // Exercise 12.4
    // Makes the list of streams streamable
  }

  implicit def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = {
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
