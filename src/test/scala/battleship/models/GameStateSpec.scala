package battleship.models

import org.scalatest._

class GameStateSpec extends FlatSpec with Matchers {
  "The GameState" should "be instantiate with 2 players" in {
    val player1 = Player("player1", Grid(10), isHuman = true)
    val player2 = Player("player2", Grid(10), isHuman = true)
    val gameState = GameState(Set(player1, player2), player1)
    gameState.getActivePlayer shouldBe player1
    gameState.getOpponentPlayer shouldBe player2
  }
  "The GameState" should "alternate players" in {
    val player1 = Player("player1", Grid(10), isHuman = true)
    val player2 = Player("player2", Grid(10), isHuman = true)
    val gameState = GameState(Set(player1, player2), player1)
    gameState.nextRound.getActivePlayer shouldBe player2
    gameState.nextRound.getOpponentPlayer shouldBe player1
  }
  "The GameState" should "have looser player" in {
    val grid1: Grid = Grid(10, Set(Ship("destroyer", Set((0, 0, true), (0, 1, true)), isSunk = true)))
    val grid2: Grid = Grid(10, Set(Ship("destroyer", Set((0, 0, false), (0, 1, false)), isSunk = true)))
    val player1: Player = Player("player1", grid1, isHuman = true)
    val player2 = Player("player2", grid2, isHuman = true)
    val gameState = GameState(Set(player1, player2), player1)
    gameState.isFinish shouldBe true
  }
  "The GameState" should "not finish" in {
    val grid1: Grid = Grid(10, Set(Ship("destroyer", Set((0, 0, false), (0, 1, false)), isSunk = true)))
    val grid2: Grid = Grid(10, Set(Ship("destroyer", Set((0, 0, false), (0, 1, false)))))
    val player1: Player = Player("player1", grid1, isHuman = true)
    val player2 = Player("player2", grid2, isHuman = true)
    val gameState = GameState(Set(player1, player2), player1)
    gameState.isFinish shouldBe false
  }
}
