import cats.implicits._
import cats._
import cats.data._

type Tracked[A] = Writer[List[String], A]
8.pure[Tracked]
List("A", "B").tell
val x = 10.writer(List("A ten"))
Writer("An eight", 8)

x.reset
x.listen

x.value
x.run

x.map(_ + 1)
x.flatMap(i => (i*2).writer(List("multiply by 2")))
val y = 15.writer(List("A Fifteen"))
val z = 20.writer(List("A Fifteen"))

//Writer is an applicative
(x, y).mapN(_ * _)

//Can combine because both List and Int has semigroup instances
x |+| y
//Logs are concatenated and values are summed up!!
Seq(x, y, z).combineAll