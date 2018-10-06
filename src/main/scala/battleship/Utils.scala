package battleship

import battleship.models.{Player, Ship}

import scala.annotation.tailrec

/**
  * Object gathering input, output methods and conversion methods
  * Used to create player's fleet
  */
object Utils {
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
        displayError("The name choosen is incorrect")
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
  def askCreateShip(player: Player, shipType: String): Option[(Int, Int, Char)] = {
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
        case 'v' | 'h' => Some((coords._1, coords._2, direction))
        case _ => None
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
    * @return the option choosen
    */
  def askOptions: Int = {
    val message = "Press the key corresponding to the instruction wanted :\n" +
      "1 - Player vs Player \n2 - Player vs Beginner AI\n" +
      "3 - Player vs Medium AI\n4 - Player vs Hard AI\n" +
      "5 - Beginner AI vs Medium AI\n6 - Medium AI vs Hard AI\n" +
      "7 - Hard AI vs Easy AI\n" + "Enter the number of the option : "
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
    printMessage(message)
    try {
      val input = scala.io.StdIn.readLine().trim().split(',')
      val position: (Int, Int) = convertPosition(input(0).charAt(0), input(1).toInt)
      if (player.grid.checkPosition(position)) position else throw new Exception
    } catch {
      case _: Throwable =>
        displayError("The position is invalid !")
        displayGridShots(player)
        askShotPosition(player)
    } finally {
      println()
    }
  }

  /**
    * Ask to the winner player if he wants restart the game
    *
    * @return the decision of the winner player
    */
  def askToRestart: Boolean = {
    val message: String = "Do you want to restart the game ?\n"
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
  def clearConsole(): Unit = println("\033c")


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
        displayGridBoats(p)
        p
      } else {
        displayGridBoats(p)
        // get the ship to create
        val ship: (String, Int) = Ship.types.toList(nbShip)
        val input: Option[(Int, Int, Char)] = askCreateShip(player, ship._1)
        // if the input is incorrect he retries
        if (input.isEmpty) {
          createFleetTailRec(p, nbShip)
        } else {
          val x: (Int, Int, Char) = input.get
          // create the ship and check (its position in the grid and no overlay with other ship)
          p.createShip(ship._1, (x._1, x._2), x._3) match {
            case None =>
              displayError("Incorrect ship")
              createFleetTailRec(p, nbShip)
            case Some(s) =>
              // add the ship to the player's grid and continue with the other
              val ships = p.grid.ships + s
              val newGrid = p.grid.copy(ships = ships)
              createFleetTailRec(p.copy(grid = newGrid), nbShip + 1)
          }
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
}