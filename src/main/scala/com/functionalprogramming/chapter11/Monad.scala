package com.functionalprogramming.chapter11

import com.functionalprogramming.chapter4.{None, Option, Some}
import com.functionalprogramming.chapter5.{Empty, Stream, Cons => StreamCons}
import com.functionalprogramming.chapter6.State
import com.functionalprogramming.chapter7.Par

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] = {
    flatMap(ma)(a => unit(f(a)))
  }

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(ma)(a => map(mb)(b => f(a, b)))
  }

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldRight(unit(List[A]()))((e, acc) => map2(e, acc)(_ :: _))
  }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = {
    sequence(la.map(f))
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(0.to(n).map(_ => ma).toList)
  }

  ///////////////////////
  // Exercises 11.5
  // ReplicateM returns a list of the given monad when the given ma is
  // of the "success" case of the flatMap and it returns an empty list
  // otherwise
  //
  // For the list monad replicateM will return one of the following:
  //   - List() when ma = List.empty
  //   - List(List(a, b, c, ...), ...) when ma = List(a, b, c, ...)
  //
  // For the option monad this will return one of the following:
  //   - None when ma = None
  //   - Some(List(a, ...)) when ma = Some(a)
  ///////////////////////

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  ///////////////////////
  // Exercises 11.6
  // Could not implement this on my own =(
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List[A]()))((a, b) =>
      compose(f, (p: Boolean) => if (p) map2(unit(a), b)(_ :: _) else b)(a)
    )
  }

  ///////////////////////

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => flatMap(f(a))(g)
  }


  ///////////////////////
  // Exercises 11.7
  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    compose((a: F[A]) => ma, f)(ma)
  }

  ///////////////////////

  ///////////////////////
  // Exercises 11.10
  // compose(f, unit) == f
  // flatMap(f)(unit) == f
  // unit(f(_)) == f
  // f == f

  // compose(unit, f) == f
  // flatMap(unit)(f) == f
  // f(unit(_)) == f
  // f == f

  // compose(f, unit) == compose(unit, f)
  // flatMap(f)(unit) == flatMap(unit)(f)
  // unit(f(_)) == f(unit(_))
  // f == f
  ///////////////////////

  ///////////////////////
  // Exercises 11.11

  // compose(f, Some) == f
  // a => flatMap(f(a))(Some) == a => f(a)
  // Some(a).flatMap(Some) == Some(a)
  // Some(a) == Some(a)

  // compose(f, Some) == f
  // a => flatMap(f(a))(Some) == a => f(a)
  // None.flatMap(Some) == None
  // None == None
  ///////////////////////

  def join[A](mma: F[F[A]]): F[A] = {
    flatMap(mma)((ma: F[A]) => ma)
  }

  def flatMapViaJoinAndMap[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    join(map(ma)(f))
  }

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => join(map(f(a))(g))
  }
}

object Monad {
  ///////////////////////
  // Exercises 11.1
  implicit val parMonad: Monad[Par] = new Monad[Par] {
    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(ma)(f)

    override def unit[A](a: => A): Par[A] = Par.unit(a)
  }

  implicit val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = {
      ma match {
        case Some(a) => f(a)
        case None => None
      }
    }
  }

  implicit val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = {
      ma match {
        case Empty => Stream.empty
        case StreamCons(hd, tl) => f(hd()).concat(flatMap(tl())(f))
      }
    }

    override def unit[A](a: => A): Stream[A] = Stream(a)
  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] = {
      ma match {
        case Nil => Nil
        case hd +: tl => List.concat(f(hd), flatMap(tl)(f))
      }
    }

    override def unit[A](a: => A): List[A] = List(a)
  }
  ///////////////////////

  ///////////////////////
  // Exercises 11.2
  // Need a way to construct state from a single A
  // This wont work
  //implicit def stateMonad[S]: Monad[State[S, _]] = new Monad[State[S, _]]
  ///////////////////////

  ///////////////////////
  // Exercises 11.17
  implicit val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] = {
      f(ma.value)
    }
  }
  ///////////////////////

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: (A) => State[S, B]): State[S, B] = ma.flatMap(f)
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = {
    Id(f(value))
  }

  def flatMap[B](f: A => Id[B]): Id[B] = {
    f(value)
  }
}
