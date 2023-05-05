import cats._
import cats.data.Validated._
import cats.implicits._
import cats.data._

5.valid[NonEmptyList[String]]
"error".invalid

validNel(5)

5.validNel[String]
"error".invalidNel[Int]

def concat[A](as: List[A], as2: List[A]): List[A] =
  as.foldRight(as2)((a, b) => a :: b)

concat(List(1,2,3), List(4))

//Checks if some valid value passes a predicate
6.validNel[String].ensure(NonEmptyChain("number is not even"))(_ % 2 == 0)
5.validNel[String].ensure(NonEmptyChain("number is not even"))(_ % 2 == 0)

//If some predicate passes, the valid value is output, otherwise the invalid
Validated.cond(true, 5, "error")
Validated.cond(false, 5, "error")
Validated.condNec(false, 5, "error") //outputs chain

5.validNel[String].getOrElse(10)
"error".validNel[String].getOrElse(10)

5.validNel[String].orElse(10.validNec[String])
"error".validNel[String].orElse(10.validNec[String])

5.validNel[String].toEither
"error".invalidNel[Int].toEither //Fails first


