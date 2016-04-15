package com.functionalprogramming.chapter5

import scala.annotation.tailrec;

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Cons(h, t) => Some(h())
    case Empty => None
  }

  def toList: List[A] = {
    @tailrec
    def loop(stream: Stream[A], acc: List[A]): List[A] = {
      stream match {
        case Cons(head, tail) => loop(tail(), acc :+ head())
        case Empty => acc
      }
    }

    loop(this, Nil)
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(head, tail) if n > 0 => Stream.cons(head(), tail().take(n - 1))
      case _ => Empty
    }
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(head, tail) if n > 0 => tail().drop(n - 1)
      case _ => this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(head, tail) if p(head()) => Stream.cons(head(), tail().takeWhile(p))
      case _ => Empty
    }
  }

  @tailrec
  final def exists(p: A => Boolean): Boolean = {
    this match {
      case Cons(head, tail) => p(head()) || tail().exists(p)
      case _ => false
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
      case _ => z
    }
  }

  def existsViaFoldRight(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Cons(head, tail) => p(head()) && tail().forAll(p)
      case _ => true
    }
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, b) =>
      if (p(a)) {
        Stream.cons(a, b)
      } else {
        Empty
      }
    )
  }

  def headOptionViaFoldRight: Option[A] = {
    foldRight(Option.empty[A])((a, b) => Some(a))
  }

  def mapViaFoldRight[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
  }

  def filterViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, b) =>
      if (p(a)) {
        Stream.cons(a, b)
      } else {
        b
      }
    )
  }

  def appendViaFoldRight[AA >: A](a: AA): Stream[AA] = {
    foldRight(Stream[AA](a))((a, b) => Stream.cons(a, b))
  }

  def concat[AA >: A](stream: Stream[AA]): Stream[AA] = {
    this match {
      case Cons(head, tail) => Stream.cons(head(), tail().concat(stream))
      case Empty => stream
    }
  }

  def flatMapViaFoldRight[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => f(a).concat(b))
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    Stream.unfold(this) {
      case Cons(head, tail) => Some(f(head()), tail())
      case Empty => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    Stream.unfold((this, n)) { case (stream, n) =>
      stream match {
        case Cons(head, tail) if n > 0 => Some(head(), (tail(), n - 1))
        case _ => None
      }
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Cons(head, tail) if p(head()) => Some(head(), tail())
      case _ => None
    }
  }

  def zipWithViaUnfold[AA >: A, B](stream: Stream[AA])(f: (A, AA) => B): Stream[B] = {
    Stream.unfold((this, stream)) {
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Some(f(aHead(), bHead()), (aTail(), bTail()))
      case _ => None
    }
  }

  def zipAllViaUnfold[B](stream: Stream[B]): Stream[(Option[A], Option[B])] = {
    Stream.unfold((this, stream)) {
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Some((Some(aHead()), Some(bHead())), (aTail(), bTail()))
      case (Cons(aHead, aTail), Empty) => Some((Some(aHead()), None), (aTail(), Empty))
      case (Empty, Cons(bHead, bTail)) => Some((None, Some(bHead())), (Empty, bTail()))
      case _ => None
    }
  }

  def startsWith[AA >: A](stream: Stream[AA]): Boolean = {
    !Stream.unfold((this, stream)) {
      case (Cons(aHead, aTail), Cons(bHead, bTail)) if aHead() == bHead() => Some(true, (aTail(), bTail()))
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Some(false, (aTail(), bTail()))
      case _ => None
    }.exists(!_)
  }

  def tails: Stream[Stream[A]] = {
    this match {
      case Cons(_, tail) => Stream.cons(this, tail().tails)
      case _ => Empty
    }
  }

  def reverse: Stream[A] = {
    def loop(stream: Stream[A], acc: Stream[A]): Stream[A] = {
      stream match {
        case Cons(head, tail) => loop(tail(), Stream.cons(head(), acc))
        case _ => acc
      }
    }

    loop(this, Empty)
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    def loop(stream: Stream[A], z: => B, acc: Stream[B]): Stream[B] = {
      stream match {
        case Cons(head, tail) => loop(tail(), f(head(), z), Stream.cons(z, acc))
        case Empty => Stream.cons(z, acc)
      }
    }

    loop(this.reverse, z, Empty)
  }

  def zip[B](stream: Stream[B]): Stream[(A, B)] = {
    Stream.unfold((this, stream)) {
      case (Cons(aHead, aTail), Cons(bHead, bTail)) => Some((aHead(), bHead()), (aTail(), bTail()))
      case _ => None
    }
  }

  def map[B](f: A => B): Stream[B] = mapViaUnfold(f)

  def find(f: A => Boolean): Option[A] = {
    this match {
      case Cons(head, tail) => if (f(head())) Some(head()) else tail().find(f)
      case Empty => None
    }
  }
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, constant(a))
  }

  def from(a: Int): Stream[Int] = {
    Stream.cons(a, from(a + 1))
  }

  private def fib(n: Int): Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 2) + fib(n - 1)
    }
  }

  def fibs: Stream[Int] = {
    def loop(n: Int): Stream[Int] = {
      Stream.cons(fib(n), loop(n + 1))
    }

    loop(0)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((value, state)) => Stream.cons(value, unfold(state)(f))
      case None => Empty
    }
  }

  def fibsViaUnfold: Stream[Int] = {
    unfold((0, 1)) { case (a, s) => Some(a, (s, a + s)) }
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some(s, s + 1))
  }

  def constantViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some(s, s))
  }

  def onesViaUnfold: Stream[Int] = {
    unfold(1)(s => Some(s, s))
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
