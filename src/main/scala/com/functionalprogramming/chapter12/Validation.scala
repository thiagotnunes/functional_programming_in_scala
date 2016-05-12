package com.functionalprogramming.chapter12

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  // Exercise 12.6
  def validationApplicativeMap2[E]: ApplicativeMap2[({type f[x] = Validation[E, x]})#f] = {
    new ApplicativeMap2[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = {
        Success(a)
      }

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Success(a), Failure(head, tail)) => Failure(head, tail)
          case (Failure(head, tail), Success(a)) => Failure(head, tail)
          case (Failure(headA, tailA), Failure(headB, tailB)) => Failure(headA, tailA ++ (headB +: tailB))
        }
      }
    }
  }

  def validationApplicativeApply[E]: ApplicativeApply[({type f[x] = Validation[E, x]})#f] = {
    new ApplicativeApply[({type f[x] = _root_.com.functionalprogramming.chapter12.Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = {
        Success(a)
      }

      override def apply[A, B](fab: Validation[E, (A) => B])(fa: Validation[E, A]): Validation[E, B] = {
        (fab, fa) match {
          case (Success(aToB), Success(a)) => Success(aToB(a))
          case (Success(aToB), Failure(head, tail)) => Failure(head, tail)
          case (Failure(head, tail), Success(a)) => Failure(head, tail)
          case (Failure(headA, tailA), Failure(headB, tailB)) => Failure(headA, tailA ++ (headB +: tailB))
        }
      }
    }
  }
}
