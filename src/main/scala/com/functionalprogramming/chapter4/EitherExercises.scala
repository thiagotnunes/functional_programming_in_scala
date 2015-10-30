package com.functionalprogramming.chapter4

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

class EitherExercises {

  def mkName(name: String): Either[String, Name] = {
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
  }

  def mkAge(age: Int): Either[String, Age] = {
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))
  }

  def mkPerson(name: String, age: Int): Either[List[String], Person] = {
    mkName(name).map2WithAccumulation(mkAge(age))(Person)
  }

}
