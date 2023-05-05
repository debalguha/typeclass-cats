package com.example.part2.state.app

import cats.implicits._

object App {
  def main(args: Array[String]): Unit = {
    val (finalState, winner) = new TicTacToe(3).playGame
    println(
      s"""Winner: $winner
         |Final board:
         |${finalState.board.show}
         |""".stripMargin)
  }
}
