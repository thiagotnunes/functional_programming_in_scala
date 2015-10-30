package com.functionalprogramming.chapter4

class OptionExercises {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(aGet), Some(bGet)) => Some(f(aGet, bGet))
      case _ => None
    }
    //or a.flatMap(a => b.map(b => f(a, b)))
  }
}
