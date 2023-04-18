package com.example

import cats.Functor

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

object FunctorStuff extends App{

  case class Person(name: String)

  class Secret[A](val value: A) {
    private def hashed: String =
      new String(
        MessageDigest.getInstance("SHA-1")
          .digest(value.toString.getBytes(StandardCharsets.UTF_8)), StandardCharsets.UTF_8
      )


    override def toString: String = hashed
  }

  object Secret {
    implicit val secretFunctor = new Functor[Secret] {
      def map[A, B](fa: Secret[A])(f: A => B): Secret[B] = new Secret(f(fa.value))
    }
  }

  def map[A, B](secret: Secret[A])(f: A => B): Secret[B] = new Secret(f(secret.value))

  def toUpper(name: Secret[String]): Secret[String] = map(name)(_.toUpperCase)

  def toLower(name: Secret[String]): Secret[String] = map(name)(_.toLowerCase)

  /*  trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }*/
  val debal = new Secret("Debal")
  println(debal)
  println(debal.value)
  println(Functor[Secret].map(debal)(_.toUpperCase).value)

  val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(a) => Some(f(a))
      case _ => None
    }
  }

  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
      case Nil => Nil
      case x::xs => List(f(x)) ++ map(xs)(f)
    }
  }
  println(listFunctor.map(List(1,2,3))(_ * 2))
  println(listFunctor.as(List(1,2,3), 10))

  println(optionFunctor.map(Some(2))(_ + 1))
}
