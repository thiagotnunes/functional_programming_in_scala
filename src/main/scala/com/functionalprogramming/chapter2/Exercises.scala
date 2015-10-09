package com.functionalprogramming.chapter2

class Exercises {
  def fib(n: Int): Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 2) + fib(n - 1)
    }
  }
}
