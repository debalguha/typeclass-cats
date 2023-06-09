package com.example

import cats._
import cats.implicits._
import cats.kernel
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.MonadTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
class BoxSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline with ScalaCheckDrivenPropertyChecks {
  val genInt: Gen[Int] = Gen.choose(1, 10)
  val genInt2: Gen[Int] = Gen.oneOf(1, 2, 5)
  val genString: Gen[String] = Gen.alphaNumStr
  val genString2: Gen[String] = Gen.numStr

  val genTuple: Gen[(Int, String)] = for {
    i <- genInt
    s <- genString
  } yield (i, s)

  val arbInt: Arbitrary[Int] = Arbitrary(genInt)
  implicit def arbBox[A](implicit arbA: Arbitrary[A]): Arbitrary[Box[A]] =
    Arbitrary(arbA.arbitrary.map(x => Box(x)))

  implicit def arbFun[A](implicit arbA: Arbitrary[A]): Arbitrary[A => A] =
    Arbitrary(arbA.arbitrary.map(a => (_: A) => a))

  test("Wrpping and unwrapping yields same value") {
    forAll(genInt2, genString) { (i: Int, s: String) =>
      assert(Box(i).value eqv i)
      assert(Box(s).value eqv s)
    }
  }

  checkAll("Eq[Box[Int]]", EqTests[Box[Int]].eqv)

  checkAll("Monad[Box[Int]]", MonadTests[Box].monad[Int, Int, Int])
}
