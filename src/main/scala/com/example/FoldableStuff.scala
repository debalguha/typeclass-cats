package com.example

import cats._
import cats.implicits._
object FoldableStuff extends App {

  trait MList[+A]

  object MList {
    def apply[A](elems: A*): MList[A] = {
      elems.foldRight(mnil[A])((a, b) => mcons(a, b))
    }
    case class MCons[+A](hd: A, tl: MList[A]) extends MList[A]

    case object MNil extends MList[Nothing]

    def mnil[A]: MList[A] = MNil

    def mcons[A](hd: A, tail: MList[A]): MCons[A] = MCons(hd, tail)

    def sum(ints: MList[Int]): Int = ints match {
      case MNil => 0
      case MCons(hd, tl) => hd + sum(tl)
    }

    def length(ints: MList[Int]): Int = ints match {
      case MNil => 0
      case MCons(_, tl) => 1 + length(tl)
    }

    def filterPositive(ints: MList[Int]): MList[Int] = {
      ints match {
        case MNil => MNil
        case MCons(hd, tl) =>
          if (hd >= 0) MCons(hd, filterPositive(tl))
          else filterPositive(tl)
      }
    }

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
  }


  import MList._

  def foldRight[A, B](list: MList[A])(z: B)(f: (A, B) => B): B = list match {
    case MNil => z
    case MCons(hd, tl) => f(hd, foldRight(tl)(z)(f))
  }

  def foldLeft[A, B](list: MList[A])(acc: B)(f: (B, A) => B): B = list match {
    case MCons(hd, tl) => foldLeft(tl)(f(acc, hd))(f)
    case MNil => acc
  }

  def sum1(ints: MList[Int]): Int = foldRight(ints)(0)((x, y) => x + y)
  def sum2(ints: MList[Int]): Int = foldLeft(ints)(0)((acc, x) => acc + x)


  def length1(ints: MList[Int]): Int = foldRight(ints)(0)((_, y) => 1 + y)
  def length2(ints: MList[Int]): Int = foldLeft(ints)(0)((acc, _) => acc + 1)

  def filterPositive1(ints: MList[Int]): MList[Int] = foldRight(ints)(MNil: MList[Int]){ (x, y) =>
    if(x > 0) MCons(x, y)
    else y
  }

  def filterPositive2(ints: MList[Int]): MList[Int] = foldLeft(ints)(MNil: MList[Int]) { (acc, y) =>
    if (y > 0) MCons(y, acc)
    else acc
  }

  println(sum(MCons(1, MCons(2, MCons(3, MCons(4, MNil))))))
  println(sum1(MCons(1, MCons(2, MCons(3, MCons(4, MNil))))))
  println(sum2(MCons(1, MCons(2, MCons(3, MCons(4, MNil))))))

  println(length(MCons(1, MCons(2, MCons(3, MCons(4, MNil))))))
  println(length1(MCons(1, MCons(2, MCons(3, MCons(4, MNil))))))
  println(length2(MCons(1, MCons(2, MCons(3, MCons(4, MNil))))))

  println(filterPositive(MCons(1, MCons(2, MCons(-3, MCons(4, MNil))))))
  println(filterPositive1(MCons(1, MCons(2, MCons(-3, MCons(4, MNil))))))
  println(filterPositive2(MCons(1, MCons(2, MCons(-3, MCons(4, MNil))))))

/*  trait Foldable[F[_]] {
    def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B
    def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

    def foldMap[A, B](fa: F[A])(f: A => B)(implicit M: Monoid[B]): B = {
      foldLeft(fa, M.empty)((b, a) => b |+| f(a)) // syntax for combine
      //foldLeft(fa, M.empty)((b, a) => M.combine(b, f(a)))
    }
  }

  def listFoldable_List(): Foldable[List] = new Foldable[List] {
    override def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B = fa match {
      case x :: xs => foldLeft(xs, f(b, x))(f)
      case Nil => b
    }

    override def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case x :: xs => f(x, Eval.defer(foldRight(xs, lb)(f)))
      case Nil => lb
    }
  }*/



  implicit val sumIntMonoid: Monoid[Int] = Monoid.instance(0, _ + _)
  println(s" listFoldable:: ${listFoldable.foldMap[Int, Int](MList(1,2,3))(_ * 2)}")

  println(sum(MList(1,2,3)))
  println(length(MList(1,2,3,4,5)))
  println(filterPositive(MList(0,1,2,-3,4,-4)))

  def sumMList(ints: MList[Int]): Int =
    ints.foldLeft(0)(_ + _)
    //Foldable[MList].foldLeft(ints, 0)(_ + _)

  def lengthMList[A](list: MList[A]): Int =
    list.foldLeft(0)((b, _) => 1 + b)

  def filterPositiveMList(ints: MList[Int]): MList[Int] = Eval.defer(
    ints.foldRight(Eval.later(mnil[Int])) { (i, evalB) =>
      if (i >= 0) evalB.map(b => MCons(i, b))
      else evalB
    }).value
  //Below foldLeft actually ends up reverting the List. Hence above foldRight is useful here
  /*    ints.foldLeft(mnil[Int])((b, a) => {
        if(a >= 0) MCons(a, b)
        else b
      })*/

  println(sumMList(MList(1, 2, 3)))
  println(lengthMList(MList(1, 2, 3, 4, 5)))
  println(filterPositiveMList(MList(0, 1, 2, -3, 4, -4)))

  println(MList(1,2,3).foldMap(_.show)) //Using Show type class, that essentially converts Ints to String followed by Int monoid to sum
  println(MList(1,2,3).foldMap(i => i * 2)) //Using Int monoid to sum up the result

  /**
   * Implement search with Foldable
   */

  def find[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Option[A] =
      fa.foldLeft(none[A])((b, a) => if(p(a)) Some(a) else b)

  println(find(MList(1,2,3,4,5))(i => i == 3))
  println(find(MList(1,2,3,4,5))(i => i > 3))

  def exists[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean =
    find(fa)(p).nonEmpty
    //fa.foldLeft(false)((b, a) => b || p(a))
  println(exists(MList(1,2,3,4,5))(i => i == 3))

  def toList[F[_]: Foldable, A](fa: F[A]): MList[A] =
    //fa.foldLeft(mnil[A])((b, a) => MCons(a, b))
    fa.foldRight(Eval.now(mnil[A]))((a, eb) => Eval.now(MCons(a, eb.value))).value

  println(toList(MList(1,2,3,4,5)))


  def forAll[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean = {
    //fa.foldLeft(false)((b, a) => b && p(a))
    val inversePred: A => Boolean = a => !p(a)
    !exists(fa)(inversePred)
  }

  println(forAll(MList(2,4,6,8))(i => i % 2 != 0))
}
