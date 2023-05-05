package com.example.part2.state.app

import cats._
import cats.implicits._

case class GameState(
  turn: Player,
  board: Board,
  plays: List[Play]
)

object GameState {
  implicit val gameStateShow: Show[GameState] = Show.show { gs =>
    s"""Turn:: ${gs.turn}
       |
       |Board:
       |${gs.board.show}
       |
       |Plays:
       |${gs.plays.reverse}
       |""".stripMargin
  }
}