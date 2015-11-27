package com.functionalprogramming.chapter7

import java.util.concurrent.{ArrayBlockingQueue, ThreadPoolExecutor, TimeUnit}

import org.specs2.mutable.Specification

class ParSpec extends Specification {
  val es = new ThreadPoolExecutor(10, 20, 1, TimeUnit.SECONDS, new ArrayBlockingQueue[Runnable](10))

  "exercises 7.5" >> {
    "sequence" >> {
      "returns par of list" in {
        Par.run(es)(Par.sequence(List(Par.unit(1), Par.unit(2), Par.unit(3)))).get() ==== List(1, 2, 3)
      }
    }
  }

  "extra exercises from section 7.3" >> {
    "sum" >> {
      "sums a list" in {
        Par.run(es)(Par.fold(List(1, 2, 3, 4))(0)(_ + _)).get() ==== 10
      }
    }

    "max" >> {
      "finds the max of a list" in {
        def max(a: Int, b: Int): Int = if (a > b) a else b
        Par.run(es)(Par.fold(List(1, 2, 3, 4))(0)(max)).get() ==== 4
      }
    }

    "countWords" >> {
      "returns the number of words over all strings" in {
        val a = Par.flatMap(Par.parMap(List("word", "two words", "three words here"))(a => a.split("\\s").length))(as => Par.fold(as)(0)(_+_))
        Par.run(es)(a).get() ==== 6
      }
    }
  }

  "flatMap" >> {
    "does not execute any block" in {
      Par.flatMap(Par.lazyUnit(1))(a => Par.unit({sys.error("do not do it dude"); 2}))
      1 ==== 1
    }
  }
}
