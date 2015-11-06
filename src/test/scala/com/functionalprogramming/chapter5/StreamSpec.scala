package com.functionalprogramming.chapter5

import org.specs2.mutable.Specification

class StreamSpec extends Specification {

  "exercises 5.1" >> {
    "toList" >> {
      "returns list from stream" in {
        Stream(1, 2, 3).toList ==== List(1, 2, 3)
      }

      "returns empty list for Empty stream" in {
        Stream.empty[Int].toList ==== List.empty[Int]
      }
    }
  }

  "exercises 5.2" >> {
    "take" >> {
      "returns the first n elements of the stream" in {
        Stream(1, 2, 3, 4, 5, 6).take(3).toList ==== List(1, 2, 3)
      }

      "returns empty stream when stream is empty" in {
        Empty.take(3) ==== Empty
      }

      "returns all the elements when stream is smaller than n" in {
        Stream(1, 2, 3).take(4).toList ==== List(1, 2, 3)
      }
    }

    "drop" >> {
      "returns the stream without the first n elements" in {
        Stream(1, 2, 3, 4, 5, 6).drop(3).toList ==== List(4, 5, 6)
      }

      "returns empty stream when stream is empty" in {
        Empty.drop(3) ==== Empty
      }

      "returns empty when dropping more than stream size" in {
        Stream(1, 2, 3).drop(4).toList ==== List.empty[Int]
      }
    }
  }

  "exercises 5.3" >> {
    "takeWhile" >> {
      "returns stream with the first elements that match the predicate" in {
        Stream(2, 4, 6, 8, 9, 10).takeWhile(_ % 2 == 0).toList ==== List(2, 4, 6, 8)
      }

      "returns empty stream when the first element does not match the predicate" in {
        Stream(3, 4, 6, 8, 10).takeWhile(_ % 2 == 0).toList ==== List.empty[Int]
      }

      "returns empty stream when empty is empty" in {
        Stream.empty[Int].takeWhile(_ % 2 == 0).toList ==== List.empty[Int]
      }
    }
  }

  "exercises 5.4" >> {
    "forAll" >> {
      "returns true when all elements suffice the predicate" in {
        Stream(2, 4, 6, 8, 10).forAll(_ % 2 == 0) ==== true
      }

      "returns false when one element does not suffice the predicate" in {
        Stream(2, 4, 5, 8, 10).forAll(_ % 2 == 0) ==== false
      }
    }
  }

  "exercises 5.5" >> {
    "takeWhileViaFoldRight" >> {
      "returns stream with the first elements that match the predicate" in {
        Stream(2, 4, 6, 8, 9, 10).takeWhileViaFoldRight(_ % 2 == 0).toList ==== List(2, 4, 6, 8)
      }

      "returns empty stream when the first element does not match the predicate" in {
        Stream(3, 4, 6, 8, 10).takeWhileViaFoldRight(_ % 2 == 0).toList ==== List.empty[Int]
      }

      "returns empty stream when empty is empty" in {
        Stream.empty[Int].takeWhileViaFoldRight(_ % 2 == 0).toList ==== List.empty[Int]
      }
    }
  }

  "exercises 5.6" >> {
    "headOptionViaFoldRight" >> {
      "returns some head when stream is not empty" in {
        Stream(1, 2, 3).headOptionViaFoldRight ==== Some(1)
      }

      "returns some head when stream is not empty" in {
        Stream.cons(1, Stream.cons(sys.error("error"), Stream.cons(3, Empty))).headOptionViaFoldRight ==== Some(1)
      }

      "returns none when stream is empty" in {
        Stream.empty.headOptionViaFoldRight ==== None
      }
    }
  }

  "exercises 5.7" >> {
    "mapViaFoldRight" >> {
      "returns mapped stream" in {
        Stream(1, 2, 3).mapViaFoldRight(_ + 1).toList ==== List(2, 3, 4)
      }

      "returns empty stream when stream is empty" in {
        Stream.empty[Int].mapViaFoldRight(_ + 1) ==== Stream.empty[Int]
      }
    }

    "filterViaFoldRight" >> {
      "returns the filtered stream" in {
        Stream(1, 2, 3, 4, 5, 6).filterViaFoldRight(_ % 2 == 0).toList ==== List(2, 4, 6)
      }

      "returns empty stream when stream is empty" in {
        Stream.empty[Int].filterViaFoldRight(_ % 2 == 0) ==== Stream.empty[Int]
      }
    }

    "appendViaFoldRight" >> {
      "returns the stream with appended element" in {
        Stream(1, 2, 3).appendViaFoldRight(4).toList ==== List(1, 2, 3, 4)
      }

      "returns one element stream when the stream is empty" in {
        Stream.empty[Int].appendViaFoldRight(1).toList ==== List(1)
      }
    }

    "concat" >> {
      "returns concatenated streams" in {
        Stream(1, 2, 3).concat(Stream(4, 5, 6)).toList ==== List(1, 2, 3, 4, 5, 6)
      }

      "returns stream when first stream is empty" in {
        Stream(1, 2, 3).concat(Stream.empty[Int]).toList ==== List(1, 2, 3)
      }

      "returns second stream when first stream is empty" in {
        Stream.empty[Int].concat(Stream(1, 2, 3)).toList ==== List(1, 2, 3)
      }

      "returns empty stream when both streams are empty" in {
        Stream.empty.concat(Stream.empty) ==== Empty
      }
    }

    "flatMapViaFoldRight" >> {
      "returns the flatMapped stream" in {
        Stream(1, 2, 3).flatMapViaFoldRight(e => Stream(e)).toList ==== List(1, 2, 3)
      }
    }
  }

  "exercises 5.8" >> {
    "constant" >> {
      "returns stream with given value repeated" in {
        Stream.constant(1).take(3).toList ==== List(1, 1, 1)
      }
    }
  }

  "exercises 5.9" >> {
    "from" >> {
      "returns stream starting from the given value" in {
        Stream.from(1).take(3).toList ==== List(1, 2, 3)
      }
    }
  }

  "exercises 5.10" >> {
    "fibs" >> {
      "returns the infinite stream of fibonacci numbers" in {
        Stream.fibs.take(6).toList ==== List(0, 1, 1, 2, 3, 5)
      }
    }
  }

  "exercises 5.11" >> {
    "unfold" >> {
      "generates stream with the given function" in {
        Stream.unfold(0)(e => Some((e + 1, e + 1))).take(2).toList ==== List(1, 2)
      }

      "stops generation when None is returns" in {
        Stream.unfold(0)(e => if (e >= 2) None else Some((e + 1, e + 1))).toList ==== List(1, 2)
      }
    }
  }

  "exercises 5.12" >> {
    "fibsViaUnfold" >> {
      "returns the infinite stream of fibonacci numbers" in {
        Stream.fibsViaUnfold.take(6).toList ==== List(0, 1, 1, 2, 3, 5)
      }
    }

    "fromViaUnfold" >> {
      "returns stream starting from the given value" in {
        Stream.fromViaUnfold(1).take(3).toList ==== List(1, 2, 3)
      }
    }

    "constantViaUnfold" >> {
      "returns stream with given value repeated" in {
        Stream.constantViaUnfold(1).take(3).toList ==== List(1, 1, 1)
      }
    }

    "onesViaUnfold" >> {
      "returns stream with ones" in {
        Stream.onesViaUnfold.take(3).toList ==== List(1, 1, 1)
      }
    }
  }

  "exercises 5.13" >> {
    "mapViaUnfold" >> {
      "returns mapped stream" in {
        Stream(1, 2, 3).mapViaUnfold(_ + 1).toList ==== List(2, 3, 4)
      }

      "returns empty stream when stream is empty" in {
        Stream.empty[Int].mapViaUnfold(_ + 1) ==== Stream.empty[Int]
      }
    }

    "takeViaUnfold" >> {
      "returns the first n elements of the stream" in {
        Stream(1, 2, 3, 4, 5, 6).takeViaUnfold(3).toList ==== List(1, 2, 3)
      }

      "returns empty stream when stream is empty" in {
        Empty.takeViaUnfold(3) ==== Empty
      }

      "returns all the elements when stream is smaller than n" in {
        Stream(1, 2, 3).takeViaUnfold(4).toList ==== List(1, 2, 3)
      }
    }

    "takeWhileViaUnfold" >> {
      "returns stream with the first elements that match the predicate" in {
        Stream(2, 4, 6, 8, 9, 10).takeWhileViaUnfold(_ % 2 == 0).toList ==== List(2, 4, 6, 8)
      }

      "returns empty stream when the first element does not match the predicate" in {
        Stream(3, 4, 6, 8, 10).takeWhileViaUnfold(_ % 2 == 0).toList ==== List.empty[Int]
      }

      "returns empty stream when empty is empty" in {
        Stream.empty[Int].takeWhileViaUnfold(_ % 2 == 0).toList ==== List.empty[Int]
      }
    }

    "zipWithViaUnfold" >> {
      "zips two streams using the given function" in {
        Stream(1, 2, 3).zipWithViaUnfold(Stream(4, 5, 6))(_ + _).toList ==== List(5, 7, 9)
      }
    }

    "zipAllViaUnfold" >> {
      "returns a stream of pairs" in {
        Stream(1, 2, 3).zipAllViaUnfold(Stream(4, 5, 6)).toList ==== List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6)))
      }

      "returns stream with nones when second stream is smaller than the first" in {
        Stream(1, 2, 3).zipAllViaUnfold(Stream(4)).toList ==== List((Some(1), Some(4)), (Some(2), None), (Some(3), None))
      }

      "returns stream with nones when first stream is smaller than the first" in {
        Stream(1).zipAllViaUnfold(Stream(4, 5, 6)).toList ==== List((Some(1), Some(4)), (None, Some(5)), (None, Some(6)))
      }

      "returns empty stream when both streams are empty" in {
        Stream.empty.zipAllViaUnfold(Stream.empty).toList ==== List.empty
      }
    }
  }

  "exercises 5.14" >> {
    "startsWith" >> {
      "returns true when first stream starts with the second" in {
        Stream(1, 2, 3).startsWith(Stream(1, 2)) ==== true
      }

      "returns false when first stream does not start with the second" in {
        Stream(1, 2, 3).startsWith(Stream(1, 3)) ==== false
      }
    }
  }

  "exercises 5.15" >> {
    "tails" >> {
      "returns the stream of suffixes" in {
        Stream(1, 2, 3).tails.toList.map(_.toList) ==== List(List(1, 2, 3), List(2, 3), List(3))
      }
    }
  }

  "exercises 5.16" >> {
    "reverse" >> {
      "returns the stream reversed" in {
        Stream(1, 2, 3).reverse.toList ==== List(3, 2, 1)
      }
    }

    "scanRight" >> {
      "returns all intermediate results" in {
        Stream(1, 2, 3).scanRight(0)(_ + _).toList ==== List(6, 5, 3, 0)
      }
    }
  }
}
