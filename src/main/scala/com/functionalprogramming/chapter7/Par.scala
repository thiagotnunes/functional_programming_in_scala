package com.functionalprogramming.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit

object Par {
  def unit[A](a: A): Par[A] = (ExecutorService) => UnitFuture(a)

  def fork[A](pa: => Par[A]): Par[A] = {
    (es: ExecutorService) => es.submit(new Callable[A] {
      override def call(): A = pa(es).get
    })
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def isCancelled: Boolean = false

    override def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get))
    }
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  // Is there a way to do this without the run?
  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = (es: ExecutorService) => {
    val af = pa(es)
    run(es)(f(af.get))
  }

  // Exercise 7.1
  // def map2[A, B, C](parA: => Par[A], parB: => Par[B])(f: (A, B) => C): Par[C] = ???

  // Exercise 7.2
  // UnitPar?
  // ForkedPar?

  // Exercise 7.3
  // def map2[A, B, C](a: (Par[A], Long, TimeUnit), b: (Par[B], Long, TimeUnit))(f: (A, B) => C): Par[C] = {
  //   (es: ExecutorService) => {
  //     val af = a._1(es)
  //     val bf = b._1(es)
  //     UnitFuture(f(af.get(a._2, a._3), bf.get(b._2, b._3)))
  //   }
  // }

  // Exercise 7.4
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  // Exercise 7.5 -> Hard!
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    // first solution - (es: ExecutorService) => UnitFuture(ps.foldLeft(List[A]())((acc, par) => acc.+:(par(es).get)))

    ps.foldLeft(unit(List.empty[A]))((acc, par) => map2(acc, par)((as, a) => as.:+(a)))
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  // Exercise 7.6 -> Hard!
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val s = sequence(as.map(asyncF((a: A) => {
      if (f(a)) {
        Some(a)
      } else {
        None
      }
    })))

    map(s)(seq => seq.filter(_.isDefined).map(_.get))
  }

  // Extra exercises from section 7.3
  def fold[A](as: List[A])(z: A)(f: (A, A) => A): Par[A] = {
    if (as.length <= 1) {
      unit(as.headOption.getOrElse(z))
    } else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(
        fork(fold(l)(z)(f)),
        fork(fold(r)(z)(f))
      )((a, b) => f(a, b))
    }
  }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {
    flatMap(pa)(a => map2(pb, pc)((b, c) => f(a, b, c)))
  }

  def map4[A, B, C, D, E](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    //flatMap(pa)(a => flatMap(pb)(b => map2(pc, pd)((c, d) => f(a, b, c, d))))
    flatMap(pa)(a => map3(pb, pc, pd)((b, c, d) => f(a, b, c, d)))
  }

  def map5[A, B, C, D, E, F](pa: Par[A], pb: Par[B], pc: Par[C], pd: Par[D], pe: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    //flatMap(pa)(a => flatMap(pb)(b => flatMap(pc)(c => map2(pd, pe)((d, e) => f(a, b, c, d, e)))))
    flatMap(pa)(a => map4(pb, pc, pd, pe)((b, c, d, e) => f(a, b, c, d, e)))
  }
}
