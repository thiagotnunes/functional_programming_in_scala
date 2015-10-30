package com.functionalprogramming.chapter4

import scala.annotation.tailrec

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

object Either {
  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  def sequence[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = {
    @tailrec
    def loop(xs: List[Either[E, A]], acc: Either[E, List[A]]): Either[E, List[A]] = {
      xs match {
        case Nil => acc
        case Right(head) :: tail => loop(tail, acc.map(list => list :+ head))
        case Left(head) :: _ => Left(head)
      }
    }

    loop(xs, Right(List()))
  }

  def traverse[E, A, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    @tailrec
    def loop(xs: List[A], acc: Either[E, List[B]]): Either[E, List[B]] = {
      xs match {
        case Nil => acc
        case head :: tail => loop(tail, f(head).flatMap(result => acc.map(list => list :+ result)))
      }
    }

    loop(xs, Right(List()))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: (Nothing) => B): Either[E, B] = this

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this

  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](b: Either[EE, B]): Either[EE, B] = b
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: (A) => B): Either[Nothing, B] = Right(f(value))

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b.map(bValue => f(value, bValue))

  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](b: Either[EE, B]): Either[EE, B] = this
}
