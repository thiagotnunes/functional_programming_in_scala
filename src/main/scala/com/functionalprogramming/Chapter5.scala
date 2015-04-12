package com.functionalprogramming

sealed trait Stream[+A] {
  def headOption: Option[A] = {
    this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def toList: List[A] = {
    this match {
      case Empty => List()
      case Cons(h, t) => h() :: t().toList
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (n > 0) Stream.cons(h(), t().take(n - 1)) else Empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (n > 0) t().drop(n - 1) else Cons(h, t)
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Empty => Empty
      case Cons(h, t) =>
        if (p.apply(h()))
          Stream.cons(h(), t().takeWhile(p))
        else
          Empty
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Empty => true
      case Cons(h, t) =>
        if (p.apply(h()))
          t().forAll(p)
        else
          false
    }
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((value, acc) => {
      if (p(value)) {
        Stream.cons(value, acc)
      } else {
        Stream.empty
      }
    })
  }

  def headOption2: Option[A] = {
    foldRight(Option.empty[A])((value, acc) => Some(value))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((value, acc) => {
      Stream.cons(f(value), acc)
    })
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((value, acc) => {
      if (p(value))
        Stream.cons(value, acc)
      else
        acc
    })
  }

  def append[B >: A](stream: => Stream[B]): Stream[B] = {
    foldRight(stream)((value, acc) => {
      Stream.cons(value, acc)
    })
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((value, acc) => {
      f(value).append(acc)
    })
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Stream.empty
    }
  }

  def from2(n: Int): Stream[Int] = {
    Stream.unfold(n)(value => Some(value, value + 1))
  }

  def constant2[A](a: A): Stream[A] = {
    Stream.unfold(a)(value => Some(value, value))
  }
}

object Fibs {
  def fibs: Stream[Int] = {
    fibsGen(0)
  }

  def fibs2: Stream[Int] = {
    Stream.unfold(0)(value => {
      Some((fibonacci(value), value + 1))
    })
  }

  private def fibsGen(n: Int): Stream[Int] = {
    Stream.cons(fibonacci(n), fibsGen(n + 1))
  }

  private def fibonacci(n: Int): Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case 2 => 1
      case _ => fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
}

object Ones {
  def ones: Stream[Int] = {
    Stream.unfold(1)(one => Some(one, one))
  }
}