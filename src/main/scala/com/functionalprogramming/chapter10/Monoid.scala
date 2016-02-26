package com.functionalprogramming.chapter10

import com.functionalprogramming.chapter4.{None, Option}
import com.functionalprogramming.chapter8.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)

    override def zero: A = m.zero
  }

  //////////////////////////////
  // Exercise 10.1
  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }
  //////////////////////////////

  //////////////////////////////
  // Exercise 10.2
  def optionMonoid[A: Monoid] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = {
      for {
        aa1 <- a1
        aa2 <- a2
      } yield {
        implicitly[Monoid[A]].op(aa1, aa2)
      }
    }

    override def zero: Option[A] = None
  }

  //////////////////////////////

  //////////////////////////////
  // Exercise 10.3
  def endoMonoid[A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = {
      a => a1(a2(a))
    }

    override def zero: (A) => A = (a: A) => a
  }

  //////////////////////////////

  //////////////////////////////
  // Exercise 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    Prop.forAll("monoidLaws", gen)(a =>
      m.op(m.zero, a) == a &&
        m.op(m.op(a, a), a) == m.op(a, m.op(a, a))
    )
  }

  //////////////////////////////

  //////////////////////////////
  // Exercise 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

  //////////////////////////////

  //////////////////////////////
  // Exercise 10.6
  // Could not figure this one out on my own
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    foldMap(as, endoMonoid[B])(f.curried)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)
  }

  //////////////////////////////

  //////////////////////////////
  // Exercise 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length == 1) {
      f(v.head)
    } else {
      val (left, right) = v.splitAt(v.length / 2)
      m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
    }
  }

  //////////////////////////////

  def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] = {
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = {
        (a.op(a1._1, a2._1), b.op(a1._2, a2._2))
      }

      override def zero: (A, B) = {
        (a.zero, b.zero)
      }
    }
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = {
      (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
      }
    }

    override def zero: Map[K, V] = Map[K, V]()
  }

  //////////////////////////////
  // Exercise 10.17
  def functionMonoid[A, B](b: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    override def op(a1: (A) => B, a2: (A) => B): (A) => B = {
      a => {
        val b1 = a1(a)
        val b2 = a2(a)
        b.op(b1, b2)
      }
    }

    override def zero: (A) => B = {
      _ => b.zero
    }
  }

  //////////////////////////////

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    Foldable.indexedSeqFoldable.foldLeft(as)(m.zero) { (acc, e) =>
      m.op(acc, Map(e -> 1))
    }
  }
}