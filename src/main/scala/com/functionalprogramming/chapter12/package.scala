package com.functionalprogramming

import com.functionalprogramming.chapter10.Monoid

package object chapter12 {
  type Applicative[F[_]] = ApplicativeMap2[F]
  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] = {
    new Applicative[({type f[x] = Const[M, x]})#f] {
      override def unit[A](a: => A): Const[M, A] = {
        M.zero
      }

      override def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] = {
        M.op(fa, fb)
      }
    }
  }
}
