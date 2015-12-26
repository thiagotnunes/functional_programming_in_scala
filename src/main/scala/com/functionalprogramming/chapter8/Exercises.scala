package com.functionalprogramming.chapter8

class Exercises {

  /////////////////////////////////////////////
  // Exercise 8.1
  // What properties specify a function that sums a List[Int]
  // What should the sum be if all elements of the list are the same value?
  //   it should be the list length times the head of the list

  // Sum of list of 0s should be 0
  // Sum of empty list should be 0
  // Sum of tail of the list should be Sum of list minus head of the list
  // Sum of two lists should be the sum of the sum of each list
  /////////////////////////////////////////////

  /////////////////////////////////////////////

  /////////////////////////////////////////////
  // Exercise 8.2
  // What properties specify a function that finds the maximum of a List[Int]?

  // Max of a list should be equal to the Max of the reverse of the list
  // Max of a list with only xs should be x
  // Max of the max of list a and the max of list b should be the max of the concatenation of those lists
  // Max of a list with one element x should be x
  // Max of an empty list should be none
  /////////////////////////////////////////////

  /////////////////////////////////////////////
  // Exercise 8.3

  trait Prop {
    def check: Boolean

    def &&(p: Prop): Prop = new Prop {
      override def check: Boolean = p.check && this.check
    }
  }

  /////////////////////////////////////////////
}
