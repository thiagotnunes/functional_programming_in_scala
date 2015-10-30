package com.functionalprogramming.chapter4

import scala.annotation.tailrec

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

object Option {
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  def sequence[A](xs: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def loop(xs: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = {
      xs match {
        case Nil => acc
        case None :: _ => None
        case Some(h) :: tail => loop(tail, acc.map(list => list :+ h))
      }
    }

    loop(xs, Some(Nil))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def loop(a: List[A], acc: Option[List[B]]): Option[List[B]] = {
      a match {
        case Nil => acc
        case h :: tail => loop(tail, f(h).flatMap(result => acc.map(list => list :+ result)))
      }
    }

    loop(a, Some(List()))
  }
}

case class Some[+A](get: A) extends Option[A] {
  override def map[B](f: (A) => B): Option[B] = Some(f(get))

  override def flatMap[B](f: (A) => Option[B]): Option[B] = f(get)

  override def filter(f: (A) => Boolean): Option[A] = {
    if (f(get)) {
      this
    } else {
      None
    }
  }

  override def getOrElse[B >: A](default: => B): B = get

  override def orElse[B >: A](ob: Option[B]): Option[B] = this
}

case object None extends Option[Nothing] {
  override def map[B](f: (Nothing) => B): Option[B] = this

  override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = this

  override def filter(f: (Nothing) => Boolean): Option[Nothing] = this

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: Option[B]): Option[B] = ob
}
