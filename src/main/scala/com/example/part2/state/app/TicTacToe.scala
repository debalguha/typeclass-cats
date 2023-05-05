package com.example.part2.state.app

import cats._
import cats.implicits._
import cats.data._

import scala.io.StdIn

class TicTacToe(boardSize: Int) {
  type Game[A] = State[GameState, A]

  val initialState: GameState = GameState(Player.P1, Board.empty(boardSize), Nil)

  def switchTurns: Game[Unit] =
    State.modify[GameState] { gameState =>
      val newTurn = if (gameState.turn === Player.P1) Player.P2 else Player.P1
      gameState.copy(turn = newTurn)
    }

  def currentPlayerWon: Game[Boolean] =
    State.get[GameState].map { gameState =>
      val board = gameState.board
      val currentPlayer = gameState.turn
      val (diag1, diag2) = board.readDiags
      List.range(0, boardSize).exists { i =>
        board.readRow(i).forall(_ === Some(currentPlayer)) ||
          board.readCol(i).forall(_ === Some(currentPlayer)) ||
          diag1.forall(_ === Some(currentPlayer)) ||
          diag2.forall(_ === Some(currentPlayer))
      }
    }

  def doPlay(play: Play): Game[Unit] = {
    State.modify[GameState] { gameState =>
      val newBoard = gameState.board.updated(play.row, play.col, gameState.turn)
      gameState.copy(board = newBoard, plays = play :: gameState.plays)
    }
  }

  def readPlayFromConsole(turn: Player): Play = {
    println(s"Enter play $turn:")
    val n = StdIn.readInt()
    Play(n / 10, n % 10)
  }

  def game: State[GameState, Player] = {
    for {
      gameState <- State.get[GameState]
      _ = println(gameState.board.show)
      play = readPlayFromConsole(gameState.turn)
      _ <- doPlay(play)
      weHaveAWinner <- currentPlayerWon
      winner <- if (weHaveAWinner) gameState.turn.pure[Game]
      else switchTurns >> game
    } yield winner
  }

  def playGame: (GameState, Player) = game.run(
    initialState
  ).value

  def replayGame_v1(plays: List[Play]): GameState = {
    def replayGameState(plays: List[Play]): Game[GameState] = plays match {
      case Nil => State.get
      case (p :: ps) => doPlay(p) >> switchTurns >> replayGameState(ps)
    }

    replayGameState(plays).runS(initialState).value
  }

  def replayGame(plays: List[Play]): GameState = {
    plays
      .foldRight(State.get[GameState])((p, g) => doPlay(p) >> switchTurns >> g)
      .runS(initialState)
      .value
  }

}
