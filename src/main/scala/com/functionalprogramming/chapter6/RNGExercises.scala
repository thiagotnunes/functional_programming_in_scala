package com.functionalprogramming.chapter6

class RNGExercises {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRng) = rng.nextInt
    if (nextInt == Int.MinValue) {
      (Int.MaxValue, nextRng)
    } else {
      (nextInt.abs, nextRng)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRng) = nonNegativeInt(rng)
    (nextInt / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextInt, nextIntRng) = nonNegativeInt(rng)
    val (nextDouble, nextDoubleRng) = double(nextIntRng)
    ((nextInt, nextDouble), nextDoubleRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (nextDouble, nextDoubleRng) = double(rng)
    val (nextInt, nextIntRng) = nonNegativeInt(nextDoubleRng)
    ((nextDouble, nextInt), nextIntRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (nextDouble1, nextDoubleRng1) = double(rng)
    val (nextDouble2, nextDoubleRng2) = double(nextDoubleRng1)
    val (nextDouble3, nextDoubleRng3) = double(nextDoubleRng2)
    ((nextDouble1, nextDouble2, nextDouble3), nextDoubleRng3)
  }

  def ints(count: Int)(seedRng: RNG): (List[Int], RNG) = {
    0.to(count).foldLeft((List.empty[Int], seedRng)) { case ((acc, rng), _) =>
      val (nextInt, nextRng) = rng.nextInt
      (acc :+ nextInt, nextRng)
    }
  }

  val int: Rand[Int] = _.nextInt

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt)(i => rng => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) {
        (mod, rng)
      } else {
        nonNegativeLessThan(n)(rng)
      }
    })
  }
}
