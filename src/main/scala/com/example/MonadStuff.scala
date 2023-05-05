package com.example

import cats._
import cats.implicits._
import com.example.MonadStuff.MOption.MNone

import scala.util.{Failure, Success, Try}
object MonadStuff extends App {
  // trait Monad[F[_]] {

  //   def pure[A](a: A): F[A]

  //   def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  //   def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
  // }

  sealed trait MOption[+A]

  object MOption {
    case class MSome[+A](a: A) extends MOption[A]

    case object MNone extends MOption[Nothing]

    implicit val monadMOption: Monad[MOption] = new Monad[MOption] {

      override def pure[A](a: A): MOption[A] = MSome(a)

      override def flatMap[A, B](fa: MOption[A])(f: A => MOption[B]): MOption[B] = fa match {
        case MSome(a) => f(a)
        case _ => MNone
      }

      override def tailRecM[A, B](a: A)(f: A => MOption[Either[A, B]]): MOption[B] = ???

      override def map[A, B](fa: MOption[A])(f: A => B): MOption[B] =
        flatMap(fa)(a => pure(f(a)))

      override def flatten[A](ffa: MOption[MOption[A]]): MOption[A] =
        flatMap(ffa)(identity)
      //flatMap(ffa)(_ => _)
    }
  }

  val x: MOption[Int] = Monad[MOption].pure(5)
  println(x)
  val y: MOption[Int] = Monad[MOption].pure(6).flatMap(i => Monad[MOption].pure(i + 1))
  println(y)

  val z: MOption[Int] = for {
    x <- Monad[MOption].pure(5)
    y <- Monad[MOption].pure(6)
  } yield x + y
  println(z)

  val k: MOption[Int] = for {
    x <- Monad[MOption].pure(5)
    y <- MNone: MOption[Int]
  } yield x + y
  println(k)


  val result = for {
    a <- List(1, 2, 3)
    b <- List(1, 2, 3)
  } yield a + b

  println(result)

  val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa match {
      case x :: xs => f(x) ++ flatMap(xs)(f)
      case Nil => List()
    }

    override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = ???

    override def pure[A](x: A): List[A] = List(x)
  }

  println(listMonad.flatMap(List(1, 2, 3))(x => List(x + 1, x + 2 )))

    //* -> * -> *
    //* -> *
    //Either is right biased, hence fixed the left side for Error and
    //reduce the kind from * -> * -> * to * -> *
    implicit def eitherMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa match {
        case Right(a) => f(a)
        case Left(s) => Left(s)
      }

      override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???

      override def pure[A](x: A): Either[E, A] = Either.right(x)
    }

  val xE: Either[String, Int] = 5.asRight[String].flatMap(i => (i + 1).asRight[String])
  val yE: Either[String, Int] = 5.asRight[String].flatMap(_ => "boom".asLeft[Int]).flatMap(_ => "boom2".asLeft[Int])
  println(xE)
  println(yE)
  val xxE: Either[String, Int] = eitherMonad[String].pure(5).flatMap(i => Either.right(i + 1))
  val yyE: Either[String, Int] = eitherMonad[String].pure(5).flatMap(_ => Either.left("Boom")).flatMap(_ => Either.left("Boom2"))
  println(xxE)
  println(yyE)

  implicit val tryMonad: Monad[Try] = new Monad[Try] {
    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa match {
      case Success(a) => f(a)
      case Failure(ff) => Failure(ff)
    }

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???

    override def pure[A](x: A): Try[A] = Success(x)
  }

  val try5 = tryMonad.pure(5)
  val try6 = try5.flatMap(i => tryMonad.pure(i + 1))
  val tryFail = try5.flatMap(_ => Failure(new RuntimeException("Illegal Try")))
  val tryFail2 = try5.flatMap(_ => Failure(new RuntimeException("Illegal Try"))).flatMap(_ => Failure(new RuntimeException("Illegal Try2")))
  println(try5)
  println(try6)
  println(tryFail)
  println(tryFail2)


}
