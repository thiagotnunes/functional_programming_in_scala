package com.functionalprogramming.chapter2

class Exercises {
  def isSorted[A](array: Array[A], f: (A, A) => Boolean): Boolean = {
    array match {
      case Array(a, b, _*) if f(a, b) => isSorted(array.tail, f)
      case Array(_) => true
      case Array() => true
      case _ => false
    }
  }

  def fib(n: Int): Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 2) + fib(n - 1)
    }
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
