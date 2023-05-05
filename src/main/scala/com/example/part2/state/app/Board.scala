package com.example.part2.state.app

import cats._
class Board private(sz: Int, board: Map[Board.Cell, Player]) {
  val size: Int = sz

  def updated(row: Int, col: Int, player: Player): Board =
    new Board(sz, board + ((row, col) -> player))

  def readCell(row: Int, col: Int): Option[Player] = board.get((row, col))

  def readRow(row: Int): List[Option[Player]] =
    List.range(0, sz).map(col => readCell(row, col))

  def readCol(col: Int): List[Option[Player]] =
    List.range(0, sz).map(row => readCell(row, col))

  def readDiags: (List[Option[Player]], List[Option[Player]]) = (
    List.range(0, sz).map(i => readCell(i, i)),
    List.range(0, sz).map(j => readCell(size - j - 1, size - j - 1))
  )
}

object Board {
  type Cell = (Int, Int)

  def empty(size: Int): Board = new Board(size, Map.empty[Cell, Player])

  implicit val showBoard: Show[Board] = Show.show { board =>
    (0 until board.size).map { row =>
      board.readRow(row).map(Player.showOptPlayer.show).mkString(" ")
    }.mkString("\n")
  }
}
