package com.functionalprogramming.chapter3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

class Exercises {
  def exercise_3_1: Int = {
    List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
  }

  def tail[A](xs: List[A]): List[A] = {
    xs match {
      case Cons(head, tail) => tail
      case Nil => Nil
    }
  }

  def setHead[A](xs: List[A], x: A): List[A] = {
    xs match {
      case Cons(_, tail) => Cons(x, tail)
      case Nil => Cons(x, Nil)
    }
  }

  @tailrec
  final def drop[A](xs: List[A], n: Int): List[A] = {
    (xs, n) match {
      case (_, 0) => xs
      case (Nil, _) => xs
      case (Cons(_, tail), _) => drop(tail, n - 1)
    }
  }

  @tailrec
  final def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = {
    xs match {
      case list@Cons(head, tail) =>
        if (f(head)) dropWhile(tail, f)
        else list
      case Nil => Nil
    }
  }

  def init[A](xs: List[A]): List[A] = {
    xs match {
      case Nil => Nil
      case Cons(head, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = {
    xs match {
      case Nil => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }
  }

  def productFoldRight(ns: List[Double]): Double = {
    foldRight(ns, 1.0)(_ * _)
  }

  def length[A](xs: List[A]): Int = {
    foldRight(xs, 0)((_, acc) => 1 + acc)
  }

  @tailrec
  final def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = {
    xs match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
  }

  def sumFoldLeft(ints: List[Int]): Int = {
    foldLeft(ints, 0)(_ + _)
  }

  def productFoldLeft(ds: List[Double]): Double = {
    foldLeft(ds, 1.0)(_ * _)
  }

  def lengthFoldLeft[A](xs: List[A]): Int = {
    foldLeft(xs, 0)((acc, _) => acc + 1)
  }

  def reverse[A](xs: List[A]): List[A] = {
    foldLeft(xs, Nil: List[A])((acc, x) => Cons(x, acc))
  }

  def append[A](xs: List[A], e: A): List[A] = {
    foldRight(xs, List(e))(Cons(_, _))
  }

  def flatten[A](xs: List[List[A]]): List[A] = {
    foldRight(xs, Nil: List[A])((x, acc) => foldRight(x, acc)(Cons(_, _)))
  }
}
