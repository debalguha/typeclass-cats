import cats._
import cats.implicits._
import cats.data._

//State == ~S => (S, A)

State[Int, Double](s => (s + 1, s.toDouble * 1))

type St[A] = State[Int, A]

8.pure[St]
8.pure[St].run(5)
8.pure[St].run(5).value

val x = State[Int, Double](s => (s + 1, s.toDouble * 4))
x.run(4)
x.run(4).value
x.runA(4).value
x.runS(4).value

State.get[Int].run(5).value
State.set[Int](10).run(5).value
State.modify[Int](s => s * 5).run(2).value
State.modify[Int](s => s * 5).run(3).value
State.inspect[Int, Int](s => s * 3).run(2).value


def get[S]: State[S, S] = State(s => (s, s))
def set[S](s: S): State[S, Unit] =
  State.set(s)
def modify[S](f: S => S): State[S, Unit] =
  State(s => (f(s), ()))
def inspect[S, T](f: S => T): State[S, T] =
  State(s => (s, f(s)))

val y = State[Int, Double](s => (s + 1, s.toDouble * 4))
val z = State[Int, Double](s => (s + 1, s.toDouble / 2))
//_.run(2) ~> (2 + 1, 2 * 4)
//_.map(d => d * 2.5) ~> (2 + 1, (2 * 4) * 2.5)
y.map(d => d * 2.5).run(2).value
//_.run(2) ~> (2 + 1, 2 * 4)
//_.flatMap(d => d * 2.5) ~> ((2 + 1) * 3, (2 * 4) + 3)
y.flatMap(d => State[Int, Double](s => (s * 2, d + 3))).run(2).value

(y, z).mapN((d1, d2) => d1 + d2).run(2).value

val t1 = State.modify[Int](s => s * 2)
val t2 = State.modify[Int](s => s * 3)

(t1 >> t2).run(5).value
(t1 >> t2 >> 8.pure[St]).run(10).value