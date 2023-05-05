package com.example

import cats._
import cats.implicits._

object ApplicativeStuff extends App {

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
      val second: Validated[B => C] = ap(first)(va)
      ap(second)(vb)
    }

    def tupledWithAP[A, B](va: Validated[A], vb: Validated[B]): Validated[(A, B)] = {
      val fun: A => B => (A, B) = a => b => (a, b)
      val fun1: Validated[A => B => (A, B)] = pure(fun)
      val fun2: Validated[B => (A, B)] = ap(fun1)(va)
      ap(fun2)(vb)
    }

    def tupledWithMAP2[A, B](va: Validated[A], vb: Validated[B]): Validated[(A, B)] = {
      map2(va, vb)((a, b) => (a, b))
    }

  }
  val v1: Validated[Int] = Applicative[Validated].pure(1)
  val v2: Validated[Int] = Applicative[Validated].pure(2)
  val v3: Validated[Int] = Applicative[Validated].pure(3)

  (v1, v2, v3).mapN((a, b, c) => a + b + c)
  (v1, v2).mapN((a, b) => a + b)

  val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = (ff, fa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }
  }

  println(optionApplicative.map2(Some(3), Some(4))(_ + _))

  val listApplicative: Applicative[List] = new Applicative[List] {
    override def pure[A](x: A): List[A] = List(x)

/*    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      fa.flatMap(a => ff.map(f => f(a)))*/

    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = (ff, fa) match {
      case (f :: fs, a :: as) => (a :: as).fmap(f) ++ ap(fs)(a :: as)
      case _ => Nil
    }
  }


  println(listApplicative.map2(List(1, 2, 3), List(4, 5, 6))(_ + _))
  println(listApplicative.map2[Int, Int, Int](List(), List(4, 5, 6))(_ + _))
}
