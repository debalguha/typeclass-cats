package com.example.part2.state.app

import cats._
import cats.implicits._
import cats.data._

sealed trait Player
case class Play(row: Int, col: Int)

object Player {
  case object P1 extends Player
  case object P2 extends Player

  implicit val showPlayer: Show[Player] = Show.show {
    case P1 => "X"
    case P2 => "O"
  }

  implicit val showOptPlayer: Show[Option[Player]] = Show.show {
    case None => "-"
    case Some(p) => p.show
  }

  implicit val eqPlayer: Eq[Player] = Eq.fromUniversalEquals[Player]
}
