package battleship

import battleship.models.{Player, Ship}

import scala.annotation.tailrec

object Utils {
  val colors: Map[String, String] = Map("red" -> (Console.RED_B+" "+Console.RESET), "blue" -> (Console.BLUE_B+" "+Console.RESET), "white" -> (Console.WHITE_B+" "+Console.RESET), "blue" -> (Console.BLUE_B+" "+Console.RESET))
  /**
    *
    * @param idPlayer
    * @return
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

  /**
    *
    */
  def clearConsole(): Unit = println("\033c")

  /**
    *
    * @return
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
    *
    * @param player
    * @return
    */
  def createFleet(player: Player): Player = {
    @tailrec
    def createFleetTailRec(p: Player, nbBoat: Int = 0): Player = {
      if (nbBoat == Ship.types.size) {
        displayGridBoats(p)
        p
      } else {
        displayGridBoats(p)
        val ship: (String, Int) = Ship.types.toList(nbBoat)
        val input: Option[(Int, Int, Char)] = askCreateShip(player, ship._1)
        if (input.isEmpty) {
          createFleetTailRec(p, nbBoat)
        } else {
          val x: (Int, Int, Char) = input.get
          p.createShip(ship._1, (x._1, x._2), x._3) match {
            case None =>
              displayError("Incorrect ship")
              createFleetTailRec(p, nbBoat)
            case Some(s) =>
              val ships = p.grid.ships + s
              val newGrid = p.grid.copy(ships = ships)
              createFleetTailRec(p.copy(grid = newGrid), nbBoat + 1)
          }
        }
      }
    }

    createFleetTailRec(player)
  }

  /**
    *
    * @param player
    */
  def displayGridShots(player: Player): Unit = {
    val g: List[List[String]] = player.grid.fillGridWithPlayerShots
    displayGridHeaderLetter(g)
    println()
    g.zipWithIndex.foreach({case(list: List[String],index: Int)=>
    {
      // insert grid header
      print(index+1)
      if (index < 9) print(" ")
      list.zipWithIndex.foreach({ case (x: String, i: Int) =>
        print(" | " + x)
        if (i+1 == list.size) print(" ")
      })
    }
      print("|\n")
    })
    displayGridHeaderSymb(g)
    println()
  }

  /**
    *
    * @param grid
    */
  def displayGridHeaderLetter(grid: List[List[String]]): Unit = {
    println()
    val limit: Char = ('A'.toInt + grid.size - 1).toChar
    ('A' to limit).zipWithIndex.foreach({ case (letter: Char, index: Int) => if (index > 0) print("   " + letter) else print("     " + letter) })
    println()
    displayGridHeaderSymb(grid)
    println()
  }

  /**
    *
    * @param grid
    */
  def displayGridHeaderSymb(grid: List[List[String]]): Unit = {
    val limit: Char = ('A'.toInt + grid.size - 1).toChar
    ('A' to limit).zipWithIndex.foreach({ case (_: Char, index: Int) => if (index > 0) print("   " + "_") else print("     _") })
  }

  /**
    *
    * @param player
    * @return
    */
  def askShotPosition(player: Player): (Int, Int) = {
    val message: String = s"Player ${player.username} - Enter the position to shoot\n(format : `X , Y`) : "
    printMessage(message)
    try {
      val input = scala.io.StdIn.readLine().trim().split(',')
      val position: (Int, Int) = convertPosition(input(0).charAt(0), input(1).toInt)
      if (player.grid.checkPosition(position)) position else {
        displayError("The position is invalid !")
        displayGridShots(player)
        askShotPosition(player)
      }
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
    *
    * @param player
    * @param shipType
    * @return
    */
  def askCreateShip(player: Player, shipType: String): Option[(Int, Int, Char)] = {
    val message: String = "Player `" + player.username + "` - Enter the position and orientation for `" +
      shipType + "` ship (size : " + Ship.types(shipType) + ")" +
      "\n(format : `X,Y,orientation`) -- v - vertical or h - horizontal"
    printMessage(message)
    try{
      val input = scala.io.StdIn.readLine().trim().split(',')
      val coords = convertPosition(input(0).charAt(0), input(1).toInt)
      val direction: Char = input(2).toLowerCase().charAt(0)
      direction match {
        case 'v' | 'h' => Some((coords._1, coords._2, direction))
        case _ => None
      }
    }catch{
      case _: Throwable =>
        displayError("invalid ship position")
        displayGridBoats(player)
        askCreateShip(player, shipType)
    } finally clearConsole()
  }

  def askToRestart: Boolean = {
    val message: String = "Do you want to restart the game ?\n"
    printMessage(message)
    try {
      val input: Char = scala.io.StdIn.readLine().trim().charAt(0)
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
    *
    * @param message
    */
  def displayError(message: String): Unit = {
    printMessage(s"${colors("red")} Error - $message ${colors("red")}")
  }

  /**
    *
    * @param player
    */
  def displayGridBoats(player: Player): Unit = {
    val shipGrid: List[List[String]] = player.grid.fillGridWithShip
    val grid: List[List[String]] = player.grid.fillGridWithOpponentShots(shipGrid)
    displayGridHeaderLetter(grid)
    println()
    grid.zipWithIndex.foreach({ case (list: List[String], index: Int) => {
      // insert grid header
      print(index + 1)
      if (index < 9) print(" ")
      list.zipWithIndex.foreach({ case (x: String, i: Int) =>
        print(" | " + x)
        if (i + 1 == list.size) print(" ")
      })
      }
      print("|\n")
    })
    displayGridHeaderSymb(grid)
    println()
  }

  /**Convert grid position
    * Ex: (A,1) to (0,0)
    * @param x String (A -> J)
    * @param y Int (1 -> 10)
    * @return
    */
  def convertPosition(x: Char, y: Int): (Int, Int) = {
    // ASCII code for A = 65
    (x.toUpper.toInt - 65, y - 1)
  }

  /**
    *
    * @param message
    */
  def printMessage(message: String): Unit = {
    println()
    println(message)
    println()
  }
}