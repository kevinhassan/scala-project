package battleship

import java.io.File

import battleship.models.{GameState, Player, Ship}
import com.github.tototoshi.csv.CSVWriter

import scala.annotation.tailrec
import scala.util.Random

/**
  * Object gathering input, output methods and conversion methods
  * Used to create player's fleet
  */
object Utils {
  val randomDirection: Random = Random
  val randomX: Random = Random
  val randomY: Random = Random
  val fileName: String = "ai_proof.csv"

  // colors used on the console
  val colors: Map[String, String] = Map("red" -> (Console.RED_B+" "+Console.RESET), "blue" -> (Console.BLUE_B+" "+Console.RESET), "white" -> (Console.WHITE_B+" "+Console.RESET), "blue" -> (Console.BLUE_B+" "+Console.RESET))
  /**
    * Ask the username to the player
    * The active player is 1 and the other 2
    *
    * @param idPlayer the number of the player concerned
    * @return the username entered
    */
  def askUsername(idPlayer: Int): String = {
    clearConsole()
    val message = s"Enter your username player $idPlayer : "
    printMessage(message)
    try{
      scala.io.StdIn.readLine()
    }catch{
      case _: NumberFormatException =>
        displayError("The name chosen is incorrect")
        askUsername(idPlayer)
    }
  }

  /** Ask to the player to create a ship
    * take coordinates and direction entered
    * Collect player's response and parse with comma
    *
    * @param player   the player concern by the creation
    * @param shipType specific ship to create
    * @return correct coordinates and direction symbol
    */
  def askCreateShip(player: Player, shipType: String): (Int, Int, Char) = {
    val message: String = "Player `" + player.username + "` - Enter the position and orientation for `" +
      shipType + "` ship (size : " + Ship.types(shipType) + ")" +
      "\n(format : `X,Y,orientation`) -- v - vertical or h - horizontal"
    printMessage(message)
    try {
      // collect response, reformat and parse with comma
      val input = scala.io.StdIn.readLine().trim().split(',')
      val coords = convertPosition(input(0).charAt(0), input(1).toInt)
      val direction: Char = input(2).toLowerCase().charAt(0)
      direction match {
        case 'v' | 'h' => (coords._1, coords._2, direction)
        case _ =>
          displayError("invalid direction")
          askCreateShip(player, shipType)
      }
    } catch {
      case _: Throwable =>
        displayError("invalid ship position")
        displayGridBoats(player)
        askCreateShip(player, shipType)
    } finally clearConsole()
  }

  /**
    * Ask to the main player to choose one option
    *
    * @return the option chosen
    */
  def askOptions: Int = {
    val message = "Press the key corresponding to the instruction wanted :\n" +
      "1 - Player vs Player \n2 - Player vs Beginner AI\n" +
      "3 - Player vs Medium AI\n4 - Player vs Hard AI\n" +
      "5 - Beginner AI vs Medium AI\n6 - Medium AI vs Hard AI\n" +
      "7 - Hard AI vs Easy AI\n8 - Launch AI simulation (save CSV)\n" + "Enter the number of the option : "
    printMessage(message)
    try{
      scala.io.StdIn.readInt()
    }catch{
      case _: NumberFormatException =>
        displayError("The option chosen is invalid")
        askOptions
    }
  }

  /**
    * Ask to the player where shoot
    * Check if position choose by the player is on the grid
    *
    * @param player who will shot the opponent grid
    * @return a good coordinate to shoot
    */
  def askShotPosition(player: Player): (Int, Int) = {
    val message: String = s"Player ${player.username} - Enter the position to shoot\n(format : `X , Y`) : "
    if (player.isHuman) {
      printMessage(message)
      try {
        val input = scala.io.StdIn.readLine().trim().split(',')
        val position: (Int, Int) = convertPosition(input(0).charAt(0), input(1).toInt)
        if (player.grid.checkPosition(position)) position else throw new Exception
      } catch {
        case _: Throwable =>
          if (player.isHuman) displayError("The position is invalid !")
          displayGridShots(player)
          askShotPosition(player)
      } finally {
        println()
      }
    } else {
      // check what type of IA it's to adopt specific strategy
      player.username match {
        case "easy" =>
          // shot randomly on the player grid
          generateRandomPosition(player)
        case "medium" =>
          // generate randomly coordinates and compare if they are not already shot before
          val position: (Int, Int) = generateRandomPosition(player)
          if (player.hasAlreadyShot(position)) askShotPosition(player) else position
        case "hard" =>
          // at the first shot, shot at the center of the grid
          if (player.grid.shots.isEmpty) {
            (player.grid.size / 2, player.grid.size / 2)
          } else {
            // get the last shot which hit a ship
            val hitShots: Set[(Int, Int, Boolean)] = player.grid.shots.filter(_._3)
            // if no shot hit a ship then shot randomly
            if (hitShots.isEmpty) {
              val position: (Int, Int) = generateRandomPosition(player)
              if (player.hasAlreadyShot(position)) askShotPosition(player) else position
            } else {
              val lastPosition: (Int, Int, Boolean) = hitShots.last
              val upPosition: (Int, Int) = (lastPosition._1, lastPosition._2 - 1)
              val downPosition: (Int, Int) = (lastPosition._1, lastPosition._2 + 1)
              val leftPosition: (Int, Int) = (lastPosition._1 - 1, lastPosition._2)
              val rightPosition: (Int, Int) = (lastPosition._1 + 1, lastPosition._2)
              if (player.grid.checkPosition(upPosition) && !player.hasAlreadyShot(upPosition)) {
                upPosition
              } else if (player.grid.checkPosition(downPosition) && !player.hasAlreadyShot(downPosition)) {
                downPosition
              } else if (player.grid.checkPosition(leftPosition) && !player.hasAlreadyShot(leftPosition)) {
                leftPosition
              } else if (player.grid.checkPosition(rightPosition) && !player.hasAlreadyShot(rightPosition)) {
                rightPosition
              } else {
                // if all position were touched or can't be touch we shot randomly on the grid
                val position: (Int, Int) = generateRandomPosition(player)
                if (player.hasAlreadyShot(position)) askShotPosition(player) else position
              }
            }
          }
      }
    }
  }

  /**
    * Ask to the winner player if he wants restart the game
    * @return the decision of the winner player
    */
  def askToRestart: Boolean = {
    val message: String = "Do you want to restart the game (y/n) ?\n"
    printMessage(message)
    try {
      val input: Char = scala.io.StdIn.readLine().trim().toLowerCase.charAt(0)
      input match {
        case 'y' => true
        case 'n' => false
        case _ => throw new Exception
      }
    } catch {
      case _: Throwable => Console
        displayError("invalid option")
        askToRestart
    } finally clearConsole()
  }

  /**
    * Clear the console
    * \033c is a special char
    */
  def clearConsole(): Unit = {
    Thread.sleep(500)
    println("\033c")
  }

  /**
    * Generate a position in the grid
    *
    * @param player owner of the grid
    * @return coordinates in the grid
    */
  def generateRandomPosition(player: Player): (Int, Int) = {
    val x = randomX.nextInt(player.grid.size)
    val y = randomY.nextInt(player.grid.size)
    (x, y)
  }

  /**
    * Generate a random direction
    * Char v or h
    *
    * @return one of two possible character
    */
  def generateRandomDirection: Char = {
    val direction: Int = randomDirection.nextInt(2)
    if (direction == 0) 'v' else 'h'
  }

  /**
    * Create the fleet for the player
    * Ask the position to place the ship and the direction
    * Check if the ship is correct (in the grid and no overlay)
    *
    * @param player the one who want to create his fleet
    * @return
    */
  def createFleet(player: Player): Player = {
    @tailrec
    def createFleetTailRec(p: Player, nbShip: Int = 0): Player = {
      // all ship are created
      if (nbShip == Ship.types.size) {
        if (player.isHuman) displayGridBoats(p)
        p
      } else {
        if (player.isHuman) displayGridBoats(p)
        // get the ship to create
        val ship: (String, Int) = Ship.types.toList(nbShip)
        // if the player is human he types himself his position else position is chosen randomly
        val input: (Int, Int, Char) = if (player.isHuman) askCreateShip(p, ship._1) else {
          val coords: (Int, Int) = generateRandomPosition(player)
          val direction: Char = generateRandomDirection
          (coords._1, coords._2, direction)
        }
        // create the ship and check (its position in the grid and no overlay with other ship)
        p.createShip(ship._1, (input._1, input._2), input._3) match {
          case None =>
            if (player.isHuman) displayError("Incorrect ship")
            createFleetTailRec(p, nbShip)
          case Some(s) =>
            // add the ship to the player's grid and continue with the other
            val ships = p.grid.ships + s
            val newGrid = p.grid.copy(ships = ships)
            createFleetTailRec(p.copy(grid = newGrid), nbShip + 1)
        }
      }
    }
    createFleetTailRec(player)
  }

  /**
    * Display to the player shots done by himself
    * Generate an empty grid and fill it with his shots
    *
    * @param player who see shots
    */
  def displayGridShots(player: Player): Unit = {
    displayGrid(player.grid.fillGridWithPlayerShots)
  }

  /**
    * Display grid
    * Add letters, separations and horizontal line
    *
    * @param grid grid to display
    */
  def displayGrid(grid: List[List[String]]): Unit = {
    println()
    val limit: Char = ('A'.toInt + grid.size - 1).toChar
    ('A' to limit).zipWithIndex.foreach({ case (letter: Char, index: Int) => if (index > 0) print("   " + letter) else print("     " + letter) })
    println()
    displayHorizontalLine(grid)
    println()
    grid.zipWithIndex.foreach({ case (list: List[String],index: Int)=>
    {
      // insert grid header
      print(index+1)
      if (index < 9) print(" ")
      list.zipWithIndex.foreach({ case (x: String, i: Int) =>
        print(" | " + x)
        if (i + 1 == list.size) print(" ")
      })
    }
      print("|\n")
    })
    displayHorizontalLine(grid)
    println()
  }

  /**
    * Display horizontal line separtor
    *
    * @param grid grid to display
    */
  def displayHorizontalLine(grid: List[List[String]]): Unit = {
    val limit: Char = ('A'.toInt + grid.size - 1).toChar
    ('A' to limit).zipWithIndex.foreach({ case (_: Char, index: Int) => if (index > 0) print("   " + "_") else print("     _") })
  }

  /**
    * Display error to the player
    *
    * @param message error message to display
    */
  def displayError(message: String): Unit = {
    printMessage(s"${colors("red")} Error - $message ${colors("red")}")
  }

  /**
    * Display the grid with opponent shots and boats
    * Fill the grid with boat and then with opponent shots
    *
    * @param player concern by the grid's display
    */
  def displayGridBoats(player: Player): Unit = {
    val shipGrid: List[List[String]] = player.grid.fillGridWithShip
    val grid: List[List[String]] = player.grid.fillGridWithOpponentShots(shipGrid)
    displayGrid(grid)
  }

  /**
    * Convert grid position
    * Ex: (A,1) to (0,0)
    *
    * @param x String (A -> J)
    * @param y Int (1 -> 10)
    * @return
    */
  def convertPosition(x: Char, y: Int): (Int, Int) = {
    // ASCII code for A = 65
    (x.toUpper.toInt - 65, y - 1)
  }

  /**
    * Print message to the player
    *
    * @param message message to print
    */
  def printMessage(message: String): Unit = {
    println()
    println(message)
    println()
  }

  /**
    * write game states of different AI on an CSV file
    *
    * @param gameStates all different game played
    */
  def writeOnCsv(gameStates: List[GameState]): Unit = {
    val file = new File(fileName)
    val writer = CSVWriter.open(file)
    writer.writeRow(List("AI Name", "score", "AI Name 2", "score 2"))
    gameStates.foreach(gameState => {
      writer.writeRow(List(s"AI Level ${gameState.getActivePlayer.username}", gameState.getActivePlayer.score, s"Level ${gameState.getOpponentPlayer.username}", gameState.getOpponentPlayer.score))
    })
    writer.close()
    printMessage("file `ai_proof.csv` successfully created at the project root")
  }
}