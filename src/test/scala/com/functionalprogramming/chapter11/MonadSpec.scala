package com.functionalprogramming.chapter11

import org.specs2.mutable.Specification

class MonadSpec extends Specification {

  "exercises 11.5" >> {
    "replicateM" >> {
      "returns empty list when an empty list is given" in {
        import Monad.listMonad
        listMonad.replicateMViaSequence(10, List()) ==== List()
      }
    }
  }

}
