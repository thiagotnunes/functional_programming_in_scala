package com.functionalprogramming.chapter12

import com.functionalprogramming.chapter10.{Foldable, Monoid}
import com.functionalprogramming.chapter11.{Functor, Monad}
import com.functionalprogramming.chapter3._
import com.functionalprogramming.chapter4.{None, Option, Some}
import com.functionalprogramming.chapter6.State

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = {
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)
  }

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = {
    traverseS(fa)((a: A) => for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _ <- State.set(s2)
    } yield b).run(s)
  }

  override def toList[A](fa: F[A]): List[A] = {
    List.reverse(mapAccum(fa, List[A]())((a, s) => ((), Cons(a, s)))._2)
  }

  // Exercise 12.16
  def reverse[A](fa: F[A]): F[A] = {
    mapAccum(fa, List.reverse(toList(fa))) { case (_, Cons(head, tail)) => (head, tail) }._1
  }


  // Exercise 12.17
  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = {
    mapAccum(as, z)((a, s) => (a, f(s, a)))._2
  }
}

object Traverse {
  implicit val listTraverse = new Traverse[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = {
      fa match {
        case Nil => Nil
        case Cons(head, tail) => Cons(f(head), map(tail)(f))
      }
    }

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = ???

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = ???

    override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = ???
  }

  implicit val optionTraverse = new Traverse[Option] {
    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = {
      fa match {
        case None => None
        case Some(a) => Some(f(a))
      }
    }

    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = ???

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = ???

    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = ???
  }

  implicit def mapTraverse[K] = new Traverse[({type f[x] = Map[K, x]})#f] {
    override def map[A, B](fa: Map[K, A])(f: (A) => B): Map[K, B] = {
      if (fa.isEmpty) {
        Map()
      } else {
        val (key, value) = fa.head
        Map((key, f(value))) ++ map(fa.tail)(f)
      }
    }

    override def foldRight[A, B](as: Map[K, A])(z: B)(f: (A, B) => B): B = ???

    override def foldLeft[A, B](as: Map[K, A])(z: B)(f: (B, A) => B): B = ???

    override def foldMap[A, B](as: Map[K, A])(f: (A) => B)(mb: Monoid[B]): B = ???
  }

  implicit val treeTraverse = new Traverse[Tree] {
    override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = {
      fa match {
        case Leaf(a) => Leaf(f(a))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }
    }
  }
}

trait TraversableFunctor[F[_]] extends Functor[F] {
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)

  // Exercise 12.14
  // Could not do it on my own, only after checking the answers
  type Id[A] = A

  val idApplicative = new Applicative[Id] {
    def unit[A](a: => A) = a

    override def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = {
      f(fa, fb)
    }
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idApplicative)
}

