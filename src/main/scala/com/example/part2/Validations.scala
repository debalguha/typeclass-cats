package com.example.part2

import cats._
import cats.implicits._
import cats.data._

object Validations extends App {
  case class Person(name: String, age: Int)
  type IsValid[A] = Validated[List[String], A]

  //Age must be on or above 18
  def validateAge(age: Int): IsValid[Int] =
    if(age >= 18) Validated.valid(age)
    else Validated.invalid(List("Must be an adult"))

  def validateName(name: String): IsValid[String] =
    if(name.nonEmpty) Validated.valid(name)
    else Validated.invalid(List("Name must not be empty "))

  def validatePerson(person: Person): IsValid[Person] =
    (validateName(person.name), validateAge(person.age)).mapN { (n, a) =>
      Person(n, a)
    }
}
