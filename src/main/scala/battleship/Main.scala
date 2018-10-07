package battleship

import battleship.models.{GameState, Grid, Player}

object Main extends App {
  val gridSize = 10
  Utils.printMessage("Welcome to the Battleship game !")
  chooseMode()

  /**
    * Select the mode to play
    */
  def chooseMode(): Unit = {
    val optionChosen: Int = Utils.askOptions
    optionChosen match {
      case 1 => playerVsPlayer()
      case 2 => playerVsIa("easy")
      case 3 => playerVsIa("medium")
      case 4 => playerVsIa("hard")
      case 5 => iaVsIa("easy", "medium")
      case 6 => iaVsIa("medium", "hard")
      case 7 => iaVsIa("hard", "easy")
      case 8 =>
        Utils.writeOnCsv(List(iaVsIa("easy", "medium")))
      case _ =>
        Utils.displayError("Incorrect option choose")
        chooseMode()
    }
  }

  /**
    * Mode player vs player
    *
    * @return
    */
  def playerVsPlayer(): GameState = {
    val username1: String = Utils.askUsername(1)
    val username2: String = Utils.askUsername(2)
    // create player with his fleet of ship
    val player1 = Utils.createFleet(Player(username1, Grid(gridSize), isHuman = true))
    Utils.clearConsole()
    val player2 = Utils.createFleet(Player(username2, Grid(gridSize), isHuman = true))
    Utils.clearConsole()
    val gameState: GameState = GameState(Set(player1, player2), player1)

    def playerVsPlayerTailRec(g: GameState): GameState = {
      // take the active player of the game
      val activePlayer: Player = g.getActivePlayer
      val opponentPlayer: Player = g.getOpponentPlayer
      // display grids
      Utils.displayGridBoats(activePlayer)
      Utils.displayGridShots(activePlayer)
      // indicate the coords where the grid will be shoot
      val coords: (Int, Int) = Utils.askShotPosition(activePlayer)
      // result of the shot done
      val isShot: Boolean = opponentPlayer.grid.isTouched(coords)
      // print the result of the shot
      if (isShot) Utils.printMessage("Hit !") else Utils.printMessage("Miss !")
      // add the shot done by the active player to the opponent
      val nOpponentPlayer: Player = opponentPlayer.takeAShot(coords)
      // add the position and its result to the active player
      val nActivePlayer: Player = activePlayer.makeAShot((coords._1, coords._2, isShot))
      // check if the opponentPlayer has at least one ship on his grid
      val nGameState: GameState = g.copy(players = Set(nActivePlayer, nOpponentPlayer))
      // check if the opponent has always a ship
      if (nGameState.isFinish) {
        // finish the game and print the gameState score
        Utils.printMessage(s"Player ${nActivePlayer.username} win !")
        Utils.printMessage(s"Player ${nActivePlayer.username} (${nActivePlayer.score + 1}) - Player ${nOpponentPlayer.username} (${nOpponentPlayer.score})")
        if (Utils.askToRestart) {
          val p1: Player = nActivePlayer.copy(score = nActivePlayer.score + 1)
          val np1: Player = Utils.createFleet(p1.resetPlayer)
          Utils.clearConsole()
          val p2: Player = Utils.createFleet(nOpponentPlayer.resetPlayer)
          playerVsPlayerTailRec(nGameState.copy(players = Set(p2, np1), launcher = p2))
        } else {
          g
        }
      } else {
        // we change the round
        playerVsPlayerTailRec(nGameState.nextRound)
      }
    }

    playerVsPlayerTailRec(gameState)
  }

  def playerVsIa(difficulty: String): GameState = {
    val username1: String = Utils.askUsername(1)
    // create player with his fleet of ship
    val player1 = Utils.createFleet(Player(username1, Grid(gridSize), isHuman = true))
    Utils.clearConsole()
    val player2 = Utils.createFleet(Player(difficulty, Grid(gridSize), isHuman = false))
    val gameState: GameState = GameState(Set(player1, player2), player1)

    def playerVsIaTailRec(g: GameState): GameState = {
      // take the active player of the game
      val activePlayer: Player = g.getActivePlayer
      val opponentPlayer: Player = g.getOpponentPlayer
      // display grids
      if (activePlayer.isHuman) {
        Utils.displayGridBoats(activePlayer)
        Utils.displayGridShots(activePlayer)
      }
      // indicate the coords where the grid will be shoot
      val coords: (Int, Int) = Utils.askShotPosition(activePlayer)
      // result of the shot done
      val isShot: Boolean = opponentPlayer.grid.isTouched(coords)
      // print the result of the shot
      if (isShot && activePlayer.isHuman) Utils.printMessage("Hit !") else if (activePlayer.isHuman) Utils.printMessage("Miss !")
      // add the shot done by the active player to the opponent
      val nOpponentPlayer: Player = opponentPlayer.takeAShot(coords)
      // add the position and its result to the active player
      val nActivePlayer: Player = activePlayer.makeAShot((coords._1, coords._2, isShot))
      // check if the opponentPlayer has at least one ship on his grid
      val nGameState: GameState = g.copy(players = Set(nActivePlayer, nOpponentPlayer))
      // check if the opponent has always a ship
      if (nGameState.isFinish) {
        // finish the game and print the gameState score
        if (nActivePlayer.isHuman) {
          Utils.printMessage(s"Player ${nActivePlayer.username} (${nActivePlayer.score + 1}) - Player IA-${nOpponentPlayer.username} (${nOpponentPlayer.score})")
        } else {
          Utils.printMessage(s"Player IA-${nActivePlayer.username} (${nActivePlayer.score + 1}) - Player ${nOpponentPlayer.username} (${nOpponentPlayer.score})")
        }
        if (Utils.askToRestart) {
          val p1: Player = nActivePlayer.copy(score = nActivePlayer.score + 1)
          val np1: Player = Utils.createFleet(p1.resetPlayer)
          Utils.clearConsole()
          val p2: Player = Utils.createFleet(nOpponentPlayer.resetPlayer)
          playerVsIaTailRec(nGameState.copy(players = Set(p2, np1), launcher = p2))
        } else {
          g
        }
      } else {
        // we change the round
        playerVsIaTailRec(nGameState.nextRound)
      }
    }
    playerVsIaTailRec(gameState)
  }

  def iaVsIa(difficultyJ1: String, difficultyJ2: String): GameState = {
    // create player with his fleet of ship
    val player1 = Utils.createFleet(Player(difficultyJ1, Grid(gridSize), isHuman = false))
    val player2 = Utils.createFleet(Player(difficultyJ2, Grid(gridSize), isHuman = false))
    val gameState: GameState = GameState(Set(player1, player2), player1)

    def iaVsIaTailRec(g: GameState, nbGame: Int = 100): GameState = {
      val activePlayer: Player = g.getActivePlayer
      val opponentPlayer: Player = g.getOpponentPlayer
      // indicate the coords where the grid will be shoot
      val coords: (Int, Int) = Utils.askShotPosition(activePlayer)
      // result of the shot done
      val isShot: Boolean = opponentPlayer.grid.isTouched(coords)
      // print the result of the shot
      if (isShot && activePlayer.isHuman) Utils.printMessage("Hit !") else if (activePlayer.isHuman) Utils.printMessage("Miss !")
      // add the shot done by the active player to the opponent
      val nOpponentPlayer: Player = opponentPlayer.takeAShot(coords)
      // add the position and its result to the active player
      val nActivePlayer: Player = activePlayer.makeAShot((coords._1, coords._2, isShot))
      // check if the opponentPlayer has at least one ship on his grid
      val nGameState: GameState = g.copy(players = Set(nActivePlayer, nOpponentPlayer))

      if (nGameState.isFinish) {
        // finish the game and print the gameState score
        Utils.printMessage(s"Player IA-${nActivePlayer.username} (${nActivePlayer.score + 1}) - Player IA-${nOpponentPlayer.username} (${nOpponentPlayer.score})")
        if (nbGame > 0) {
          val p1: Player = nActivePlayer.copy(score = nActivePlayer.score + 1)
          val np1: Player = Utils.createFleet(p1.resetPlayer)
          Utils.clearConsole()
          val p2: Player = Utils.createFleet(nOpponentPlayer.resetPlayer)
          iaVsIaTailRec(nGameState.copy(players = Set(p2, np1), launcher = p2), nbGame - 1)
        } else {
          g
        }
      } else {
        // we change the round
        iaVsIaTailRec(nGameState.nextRound, nbGame)
      }
    }

    iaVsIaTailRec(gameState)
  }
}
