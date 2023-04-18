package com.example

import cats.{Applicative, Apply}

object ApplicativeStuff {

  sealed trait Validated[+A]
  case class Valid[+A](a: A) extends Validated[A]
  case class Invalid(errors: List[String]) extends Validated[Nothing]

  def validateName(name: String): Validated[String] = {
    if(name.forall(_.isLetter)) Valid(name)
    else Invalid(List("Only letters allowed"))
  }

  def validateAge(age: Int): Validated[Int] = {
    if(age < 18) Invalid(List("Must be at least 18"))
    else Valid(age)
  }

  case class Person(name: String, age: Int)

  def validatePerson(person: Person): Validated[Person] = (validateAge(person.age), validateName(person.name)) match {
    case (Valid(n), Valid(s)) => Valid(person)
    case (Invalid(errors), Valid(s)) => Invalid(errors)
    case (Valid(n), Invalid(errors)) => Invalid(errors)
    case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
  }

  def map2[A, B, C](va: Validated[A], vb: Validated[B])(f: (A, B) => C): Validated[C] = (va, vb) match {
    case (Valid(a), Valid(b)) => Valid(f(a, b))
    case (Invalid(errors), Valid(s)) => Invalid(errors)
    case (Valid(n), Invalid(errors)) => Invalid(errors)
    case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
  }

/*  trait Applicative[F[_]] extends Apply[F] {
    def pure[A](x: A): F[A]
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
  }*/

  implicit def applicative: Applicative[Validated] = new Applicative[Validated] {
    override def pure[A](x: A): Validated[A] = Valid(x)

    override def ap[A, B](vf: Validated[A => B])(va: Validated[A]): Validated[B] = (vf, va) match {
      case (Valid(f), Valid(a)) => Valid(f(a))
      case (Invalid(e1), Valid(_)) => Invalid(e1)
      case (Valid(_), Invalid(e1)) => Invalid(e1)
      case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
    }

    override def map2[A, B, C](va: Validated[A], vb: Validated[B])(f: (A, B) => C): Validated[C] = (va, vb) match {
      case (Valid(a), Valid(b)) => Valid(f(a, b))
      case (Invalid(e1), Valid(_)) => Invalid(e1)
      case (Valid(_), Invalid(e1)) => Invalid(e1)
      case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
    }

    def apByMap2[A, B](vf: Validated[A => B])(va: Validated[A]): Validated[B] = {
       map2(vf, va)((f, a) => f(a))
    }

    def map2ByApAndPure[A, B, C](va: Validated[A], vb: Validated[B])(f: (A, B) => C): Validated[C] = {
      val fun: A => B => C = a => b => f(a, b)
      val first: Validated[A => B => C] = pure(fun)
      val second: Validated[B => C] = ap(pure(fun))(va)
      ap(second)(vb)
    }
  }

}
