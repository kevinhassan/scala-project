package battleship

import battleship.models.{GameState, Grid, Player}

object Main extends App {
  val gridSize = 10
  Utils.printMessage("Welcome to the Battleship game !")
  chooseMode()

  def playerVsIa(difficulty: String): GameState = {
    val username1: String = Utils.askUsername(1)
    // create player with his fleet of ship
    val player1 = Utils.createFleet(Player(username1, Grid(gridSize), isHuman = true))
    Utils.clearConsole()
    val player2 = Utils.createFleet(Player(difficulty, Grid(gridSize), isHuman = false))
    val gameState: GameState = GameState(Set(player1, player2), player1)

    def playerVsIaTailRec(g: GameState): GameState = {
      // take the active player of the game
      //Utils.clearConsole()
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
        Utils.printMessage(s"Player `${nActivePlayer.username}` - win ! ${nActivePlayer.score + 1} - ${nOpponentPlayer.score}")
        if (Utils.askToRestart) {
          val p1: Player = Utils.createFleet(nActivePlayer.resetPlayer)
          //Utils.clearConsole()
          val p2: Player = Utils.createFleet(nOpponentPlayer.resetPlayer)
          g.copy(players = Set(p2, p1), launcher = p2)
        } else {
          Utils.printMessage("Goodbye !\n")
          g
        }
      } else {
        // we change the round
        playerVsIaTailRec(nGameState.nextRound)
      }
    }

    playerVsIaTailRec(gameState)
  }

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
      //Utils.clearConsole()
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
        Utils.printMessage(s"Player `${nActivePlayer.username}` - win ! ${nActivePlayer.score + 1} - ${nOpponentPlayer.score}")
        if (Utils.askToRestart) {
          val p1: Player = Utils.createFleet(nActivePlayer.resetPlayer)
          //Utils.clearConsole()
          val p2: Player = Utils.createFleet(nOpponentPlayer.resetPlayer)
          g.copy(players = Set(p2, p1), launcher = p2)
        } else {
          Utils.printMessage("Goodbye !\n")
          g
        }
      } else {
        // we change the round
        playerVsPlayerTailRec(nGameState.nextRound)
      }
    }

    playerVsPlayerTailRec(gameState)
  }
}
