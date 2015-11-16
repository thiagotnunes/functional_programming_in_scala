package com.functionalprogramming.chapter6

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

case class IdentityRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    (seed.toInt, this)
  }
}
