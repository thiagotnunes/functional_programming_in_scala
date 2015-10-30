package com.functionalprogramming.chapter4

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

class PersonExercises {

  def mkName(name: String): Validation[String, Name] = {
    if (name == "" || name == null) Failure("Name is empty.")
    else Success(new Name(name))
  }

  def mkAge(age: Int): Validation[String, Age] = {
    if (age < 0) Failure("Age is out of range.")
    else Success(new Age(age))
  }

  def mkPerson(name: String, age: Int): Validation[List[String], Person] = {
    mkName(name).map2(mkAge(age))(Person)
  }

}
