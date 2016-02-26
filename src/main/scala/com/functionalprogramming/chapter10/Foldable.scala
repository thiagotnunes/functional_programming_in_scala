package com.functionalprogramming.chapter10

import com.functionalprogramming.chapter3.{Leaf, Branch, Tree}
import com.functionalprogramming.chapter5.{Cons, Empty, Stream}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = foldLeft(fa)(List.empty[A])((acc, a) => acc :+ a)
}

object Foldable {
  implicit val listFoldable: Foldable[List] = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
      as match {
        case List() => z
        case head +: tail => f(head, foldRight(tail)(z)(f))
      }
    }

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case List() => z
        case head +: tail => foldLeft(tail)(f(z, head))(f)
      }
    }

    override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = {
      as match {
        case List() => mb.zero
        case head +: tail => mb.op(f(head), foldMap(tail)(f)(mb))
      }
    }

    override def toList[A](fa: List[A]): List[A] = {
      fa
    }
  }

  implicit val indexedSeqFoldable: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = {
      as match {
        case Seq() => z
        case head +: tail => f(head, foldRight(tail)(z)(f))
      }
    }

    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case Seq() => z
        case head +: tail => foldLeft(tail)(f(z, head))(f)
      }
    }

    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B = {
      as match {
        case Seq() => mb.zero
        case head +: tail => mb.op(f(head), foldMap(tail)(f)(mb))
      }
    }
  }

  implicit val streamFoldable: Foldable[Stream] = new Foldable[Stream] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = {
      as match {
        case Empty => z
        case Cons(head, tail) => f(head(), foldRight(tail())(z)(f))
      }
    }

    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case Empty => z
        case Cons(head, tail) => foldLeft(tail())(f(z, head()))(f)
      }
    }

    override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B = {
      as match {
        case Empty => mb.zero
        case Cons(head, tail) => mb.op(f(head()), foldMap(tail())(f)(mb))
      }
    }
  }

  implicit val treeFoldable: Foldable[Tree] = new Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
      as match {
        case Leaf(v) => f(v, z)
        case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
      }
    }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
      as match {
        case Leaf(v) => f(z, v)
        case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
      }
    }

    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = {
      as match {
        case Leaf(v) => f(v)
        case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      }
    }
  }

  implicit val optionFoldable: Foldable[Option] = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = {
      as match {
        case Some(v) => f(v, z)
        case None => z
      }
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = {
      //as.map(v => f(z, v)).getOrElse(z)
      as match {
        case Some(v) => f(z, v)
        case None => z
      }
    }

    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = {
      as match {
        case Some(v) => f(v)
        case None => mb.zero
      }
    }
  }
}