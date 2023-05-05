package com.example

import cats._
import cats.implicits._
import cats.data._
object TraverseStuff extends App {
  trait MList[+A] {
    def map[B](f: A => B): MList[B]
  }

  object MList {
    def apply[A](elems: A*): MList[A] = {
      elems.foldRight(mnil[A])((a, b) => mcons(a, b))
    }

    case class MCons[+A](hd: A, tl: MList[A]) extends MList[A] {
      override def map[B](f: A => B): MList[B] = MCons(f(hd), tl.map(f))
    }

    case object MNil extends MList[Nothing] {
      override def map[B](f: Nothing => B): MList[B] = this
    }

    def mnil[A]: MList[A] = MNil

    def mcons[A](hd: A, tail: MList[A]): MCons[A] = MCons(hd, tail)

  }


  import MList._

  implicit val listFoldable: Foldable[MList] = new Foldable[MList] {
    override def foldLeft[A, B](fa: MList[A], b: B)(f: (B, A) => B): B = fa match {
      case MCons(h, t) => foldLeft(t, f(b, h))(f)
      case _ => b
    }

    override def foldRight[A, B](fa: MList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(as: MList[A]): Eval[B] = as match {
        case MCons(h, t) => f(h, Eval.defer(foldRight(t, lb)(f)))
        case MNil => lb
      }

      Eval.defer(loop(fa))
    }
  }

  implicit val listTraverse: Traverse[MList] = new Traverse[MList] {

    override def traverse[G[_] : Applicative, A, B](fa: MList[A])(f: A => G[B]): G[MList[B]] = fa match {
      case MNil => Applicative[G].pure(MNil)
      case MCons(h, t) => (f(h), traverse(t)(f)).mapN((a, b) => MCons(a, b))
    }

    override def foldLeft[A, B](fa: MList[A], b: B)(f: (B, A) => B): B = fa match {
      case MCons(h, t) => foldLeft(t, f(b, h))(f)
      case _ => b
    }

    override def foldRight[A, B](fa: MList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(as: MList[A]): Eval[B] = as match {
        case MCons(h, t) => f(h, Eval.defer(foldRight(t, lb)(f)))
        case MNil => lb
      }
      Eval.defer(loop(fa))
    }

    def sequence0[G[_], A](fga: MList[G[A]])(implicit ev: Applicative[G]): G[MList[A]] =
      traverse(fga)(identity)
  }

  implicit val mListFunctor = new Functor[MList] {
    override def map[A, B](fa: MList[A])(f: A => B): MList[B] = fa match {
      case MNil => mnil[B]
      case MCons(h, t) => MCons(f(h), map(t)(f))
    }
  }

  // Traverse with a functor and sequence!!
  def traverse1[G[_] : Applicative, A, B](fa: MList[A])(f: A => G[B]): G[MList[B]] = {
    listTraverse.sequence(fa.map(f))
  }

/*  trait Traverse[F[_]] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = ???
    def foldLeft[A, B](fa: F[B], b: B)(f: (B, A) => B): F[B]
    def foldRight[A, B](fa: F[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B])

  }*/
  println(Traverse[MList].sequence(MList(Option(5), Option(3), Option(2))))
  println(Traverse[MList].sequence(MList(Option(5), None, Option(2))))

  println(Traverse[MList].traverse(MList(1,2,3))(i => Option(i + 10)))

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = fa match {
      case None => Applicative[G].pure(none)
      case Some(a) => f(a).map(Some.apply)
    }

    override def foldLeft[A, B](fa: Option[A], b: B)(f: (B, A) => B): B = fa match {
      case None => b
      case Some(a) => f(b, a)
    }

    override def foldRight[A, B](fa: Option[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case None => lb
      case Some(a) => f(a, lb)
    }
  }
  println(optionTraverse.traverse(Some(5))(x => List(x + 1, x + 2)))
  println(optionTraverse.traverse(Some(5))(_ => List()))
  println(optionTraverse.traverse[List, Int, Int](None)(x => List(x + 1, x + 2)))
}
