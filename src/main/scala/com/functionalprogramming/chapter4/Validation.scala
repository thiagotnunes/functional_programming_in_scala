package com.functionalprogramming.chapter4

sealed trait Validation[+E, +A] {
  def map[B](f: A => B): Validation[E, B]

  def flatMap[EE >: E, B](f: A => Validation[EE, B]): Validation[EE, B]

  def orElse[EE >: E, B >: A](b: Validation[EE, B]): Validation[EE, B]

  def map2[EE >: E, B, C](b: Validation[EE, B])(f: (A, B) => C): Validation[List[EE], C]
}

object Validation {
  def sequence[E, A](xs: List[Validation[E, A]]): Validation[List[E], List[A]] = {
    def loop(xs: List[Validation[E, A]], failureAcc: List[E], successAcc: List[A]): Validation[List[E], List[A]] = {
      xs match {
        case Nil if failureAcc.nonEmpty => Failure(failureAcc)
        case Nil => Success(successAcc)
        case Success(value) :: tail => loop(tail, failureAcc, successAcc :+ value)
        case Failure(value) :: tail => loop(tail, failureAcc :+ value, successAcc)
      }
    }

    loop(xs, List(), List())
  }

  def traverse[E, A, B](xs: List[A])(f: A => Validation[E, B]): Validation[List[E], List[B]] = {
    def loop(xs: List[A], failureAcc: List[E], successAcc: List[B]): Validation[List[E], List[B]] = {
      xs match {
        case Nil if failureAcc.nonEmpty => Failure(failureAcc)
        case Nil => Success(successAcc)
        case head :: tail => f(head) match {
          case Success(value) => loop(tail, failureAcc, successAcc :+ value)
          case Failure(value) => loop(tail, failureAcc :+ value, successAcc)
        }
      }
    }

    loop(xs, List(), List())
  }
}

case class Failure[+E](value: E) extends Validation[E, Nothing] {
  override def map[B](f: (Nothing) => B): Validation[E, B] = this

  override def map2[EE >: E, B, C](b: Validation[EE, B])(f: (Nothing, B) => C): Validation[List[EE], C] = {
    b match {
      case Failure(bValue) => Failure(List(value, bValue))
      case Success(_) => Failure(List(value))
    }
  }

  override def flatMap[EE >: E, B](f: (Nothing) => Validation[EE, B]): Validation[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](b: Validation[EE, B]): Validation[EE, B] = b
}

case class Success[+A](value: A) extends Validation[Nothing, A] {
  override def map[B](f: (A) => B): Validation[Nothing, B] = Success(f(value))

  override def map2[EE >: Nothing, B, C](b: Validation[EE, B])(f: (A, B) => C): Validation[List[EE], C] = {
    b match {
      case Failure(bValue) => Failure(List(bValue))
      case Success(bValue) => Success(f(value, bValue))
    }
  }

  override def flatMap[EE >: Nothing, B](f: (A) => Validation[EE, B]): Validation[EE, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](b: Validation[EE, B]): Validation[EE, B] = this
}
