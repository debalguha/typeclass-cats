import scala.annotation.tailrec
import scala.util.control.TailCalls.TailRec

def fact(n: Int): Int =
  if(n == 0) 1
  else n * fact(n-1)

@tailrec
def factT(n: Int, acc: Int = 1): Int =
  if(n == 0) acc
  else factT(n-1, n * acc)

fact(5)
factT(10)

trait Trampoline[+A]
object Trampoline {
  case class Done[A](a: A) extends Trampoline[A]
  case class More[A](f: () => Trampoline[A]) extends Trampoline[A]

  @tailrec
  def run[A](ta: Trampoline[A]): A = ta match {
    case Done(a) => a
    case More(thunk) => run(thunk())
  }
}

object X {
  import Trampoline._
  def isEven_old(n: Int): Boolean =
    if(n == 0) true
    else isOdd_old(n-1)

  def isOdd_old(n: Int): Boolean =
    if(n == 0) false
    else isEven_old(n -1)

  def isEven(n: Int): Trampoline[Boolean] =
    if (n == 0) Done(true)
    else More(() => isOdd(n - 1))

  def isOdd(n: Int): Trampoline[Boolean] =
    if (n == 0) Done(false)
    else More(() => isEven(n - 1))
}

import X._
import Trampoline._
run(isEven(5))
run(isEven(6))


