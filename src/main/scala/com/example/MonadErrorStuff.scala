package com.example

import cats._
import cats.implicits._

import java.io.IOException
import scala.util.{Failure, Success, Try}
object MonadErrorStuff extends App {
  trait HttpMethod

  case object GET extends HttpMethod

  case class HttpRequest(method: HttpMethod, url: String)

  case class HttpResponse(status: Int)

  def doRequest(req: HttpRequest): HttpResponse =
    if (math.random() < 0.5) throw new IOException("boom!")
    else HttpResponse(200)

  def executeRequest(req: HttpRequest): Option[HttpResponse] =
    try {
      Some(doRequest(req))
    } catch {
      case _: Exception => None
    }

  def executeRequest2(req: HttpRequest): Either[String, HttpResponse] =
    try {
      Right(doRequest(req))
    } catch {
      case _: Exception => Left("Sorry :(")
    }

  def executeRequest3(req: HttpRequest): Try[HttpResponse] =
    try {
      Success(doRequest(req))
    } catch {
      case e: Exception => Failure(e)
    }
  println(executeRequest(HttpRequest(GET, "www.example.com")))
  println(executeRequest2(HttpRequest(GET, "www.example.com")))
  println(executeRequest3(HttpRequest(GET, "www.example.com")))

  val optionME: MonadError[Option, Unit] = new MonadError[Option, Unit] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???

    override def raiseError[A](e: Unit): Option[A] = None // Just indicates there is an error!!

    override def handleErrorWith[A](fa: Option[A])(f: Unit => Option[A]): Option[A] = fa.orElse(f())

    override def pure[A](x: A): Option[A] = Some(x)
  }

  def eitherME[E]: MonadError[Either[E, *], E] = new MonadError[Either[E, *], E] {
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???

    override def raiseError[A](e: E): Either[E, A] = Either.left(e)

    override def handleErrorWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] = fa match {
      case Right(a) => Right(a)
      case Left(e) => f(e)
    }

    override def pure[A](x: A): Either[E, A] = Either.right(x)
  }

  val tryMe: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {
    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???

    override def raiseError[A](e: Throwable): Try[A] = Failure(e)

    override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = fa match {
      case Success(a) => Success(a)
      case Failure(e) => f(e)
    }

    override def pure[A](x: A): Try[A] = Success(x)
  }

  private def executeRequestME[F[_]](req: HttpRequest)(implicit me: MonadError[F, Throwable]): F[HttpResponse] = {
    try {
      me.pure(doRequest(req))
    } catch {
      case e: Exception => me.raiseError(e)
    }
  }

  //Further generalization over "Throwable" as Option does not support second param.
  //It has to be Unit for Option
  private def executeRequestME2[F[_], E](req: HttpRequest)(f: Exception => E)(implicit me: MonadError[F, E]): F[HttpResponse] = {
    try {
      me.pure(doRequest(req))
    } catch {
      case e: Exception => me.raiseError(f(e))
    }
  }

  println(s"Monad Error instance Try:: ${executeRequestME[Try](HttpRequest(GET, "wwwww.example.com"))}")
  println(s"Monad Error instance Either:: ${executeRequestME[Either[Throwable, *]](HttpRequest(GET, "wwwww.example.com"))}")

  println(s"Monad Error2 instance Option:: ${executeRequestME2[Option, Unit](HttpRequest(GET, "wwwww.example.com"))(e => e.printStackTrace())}")
  println(s"Monad Error2 instance Try:: ${executeRequestME2[Try, Throwable](HttpRequest(GET, "wwwww.example.com"))(identity)}")
  println(s"Monad Error2 instance Either:: ${executeRequestME2[Either[Throwable, *], Throwable](HttpRequest(GET, "wwwww.example.com"))(identity)}")

  //An Either wrapped in an Option, which is always a Some!!
  //The idea is to keep the top level abstraction succeed always, while
  //The nested one carries actual computation outcome!!
  println(s"Monad Error3 attempt option:: ${MonadError[Option, Unit].attempt(Some(5))}")

  //Same as above the top level is now changed to a Try, but the nested one still remains an Either.
  //Here the top level outcome is Success always.
  println(s"Monad Error3 attempt Either:: ${MonadError[Try, Throwable].attempt(Failure(new IllegalStateException("Boom!!")))}")


  println(s"Monad Error3 ensure Option:: ${MonadError[Option, Unit].ensure(Some(4))(())( _ % 2 == 0)}")
  //println(MonadError[Option, Unit].ensure(Some(4))(e => println(e))( _ % 2 == 0))

}
