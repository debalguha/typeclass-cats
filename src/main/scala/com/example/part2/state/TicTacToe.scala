package com.example.part2.state

import cats._
import cats.implicits._
import cats.data._


object TicTacToe extends App {
  sealed trait Player
  case object P1 extends Player
  case object P2 extends Player

  class Board private(sz: Int, board: Map[Board.Cell, Player]) {
    val size = sz
    def updated(row: Int, col: Int, player: Player): Board = ???
    def readCell(row: Int, col: Int): Option[Player] = ???
    def readRow(row: Int): List[Option[Player]] = ???
    def readCol(col: Int): List[Option[Player]] = ???
    def readDiags: (List[Option[Player]], List[Option[Player]]) = ???
  }

  private object Board {
    private type Cell = (Int, Int)

    def empty(size: Int): Board =
      new Board(size, Map.empty[Cell, Player])
  }
  case class Play(row: Int, col: Int)

  class TicTacToe_v1(boardSize: Int) {
    var board: Board = Board.empty(boardSize)
    private var turn: Player = P1
    def currentPlayerOwn(): Boolean = ???
    def switchTurn(): Unit =
      turn = if(turn == P1) P2 else P1
    def doPlay(play: Play): Unit =
      board = board.updated(play.row, play.col, turn)
  }

  case class GameState(turn: Player, board: Board)
  class TicTacToe_V2(boardSize: Int) {
    def currentPlayerOwn(gameState: GameState): (GameState, Boolean) = ???

    def switchTurn(gameState: GameState): (GameState, Unit) =
      (gameState.copy(turn = if(gameState.turn == P1) P2 else P1), ())

    def doPlay(play: Play, gameState: GameState): (GameState, Unit) =
      (gameState.copy(board = gameState.board.updated(play.row, play.col, gameState.turn)), ())
  }

  //Uses State Monad
  class TicTacToe(boardSize: Int) {
    def currentPlayerOwn(gameState: GameState): State[GameState, Unit] = ???

    def switchTurn: State[GameState, Unit] =
      State.modify[GameState] { gameState =>
        val newTurn = if(gameState.turn == P1) P2 else P1
        gameState.copy(turn = newTurn)
      }

    def doPlay(play: Play): State[GameState, Unit] =
      State.modify[GameState] { gameState =>
        val newBoard = gameState.board.updated(play.row, play.col, gameState.turn)
        gameState.copy(board = newBoard)
      }
  }
}
