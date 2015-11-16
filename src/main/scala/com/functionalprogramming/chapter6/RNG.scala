package com.functionalprogramming.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}
