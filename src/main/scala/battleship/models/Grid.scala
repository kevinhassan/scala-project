package battleship.models

import battleship.Utils

import scala.annotation.tailrec
import scala.collection.immutable.Set

/**
  * Grid class manage ships and shots
  *
  * It can constitute grid thanks to ships and opponent shots
  * It can create grid with owner shoots
  *
  * @param size          grid size (>0)
  * @param ships         collection of ships
  * @param shots         collection of shot done by the grid's owner
  * @param opponentShots collection of shot done by the opponent player
  */
case class Grid(size: Int, ships: Set[Ship] = Set(), shots: Set[(Int, Int, Boolean)] = Set(), opponentShots: Set[(Int, Int, Boolean)] = Set()) {

  /**
    * Get all ships alive for the grid's owner
    *
    * @return Set[Ship] Collection of ships not sink
    */
  def getAliveShips: Set[Ship] = ships.filter(ship => !ship.isSunk)

  /**
    * Make the shot on the opponent grid
    *
    * @param position where the shot is done
    * @return Grid new grid with the shot added to shots collection
    */
  def makeAShot(position: (Int, Int, Boolean)): Grid = {
    copy(shots = shots + position)
  }

  /**
    * Take the shot on the grid
    *
    * @param position where the opponent shot
    * @return new grid with the shot added to the opponentShot collection
    */
  def takeAShot(position: (Int, Int)): Grid = {
    // if a ship is concerned by the shot then destroy ship's position and create a new one
    if (isTouched(position)) {
      // separate sunk and not sunk ship to avoid useless browse on the ship collection
      val shipsNotSunk: Set[Ship] = ships.filter { ship: Ship => !ship.isSunk }
      val shipsSunk: Set[Ship] = ships.filter { ship: Ship => ship.isSunk }
      // the new ship is updated in the not sunk ship collection
      val newShips: Set[Ship] = shipsNotSunk.map { ship => ship.destroyPosition(position) }
      // concatenate sunk ship collection to the new collection `newShips`
      copy(ships = newShips ++ shipsSunk, opponentShots = opponentShots + Tuple3(position._1, position._2, true))
    } else {
      // else the shot was not targeting a ship and the result is `false`
      copy(opponentShots = opponentShots + Tuple3(position._1, position._2, false))
    }
  }

  /**
    * Check if one ship is touch by the shot
    * Browse ships collection
    * If true then a ship is concerned by the shot
    *
    * @param position possible ship position
    * @return
    */
  def isTouched(position: (Int, Int)): Boolean = ships.exists(_.isShot(position))

  /**
    * Fill the grid with player shots
    *
    * @return grid fill with player' shots
    */
  def fillGridWithPlayerShots: List[List[String]] = {
    val symbs: Map[Boolean, String] = Map(true -> battleship.Utils.colors("red"), false -> battleship.Utils.colors("blue"))

    @tailrec
    def fillGridWithPlayerShotsTailRec(grid: List[List[String]], s: Set[(Int, Int, Boolean)], x: Int = 0, currentPos: Int = 0): List[List[String]] = {
      if (x == s.size) {
        grid
      }
      else {
        val position: (Int, Int, Boolean) = s.toList(x)
        val newGrid: List[List[String]] = grid.updated(position._2, editLine(grid(position._2), position, symbs))
        if (currentPos == s.size - 1) fillGridWithPlayerShotsTailRec(newGrid, s, x + 1) else fillGridWithPlayerShotsTailRec(newGrid, s, x, currentPos + 1)
      }
    }

    fillGridWithPlayerShotsTailRec(generateEmptyGrid, shots)
  }

  /**
    * Fill the grid with the opponent shots
    *
    * @param grid player grid to fill
    * @return new grid with opponent shots
    */
  def fillGridWithOpponentShots(grid: List[List[String]]): List[List[String]] = {
    val symbs: Map[Boolean, String] = Map(true -> Utils.colors("red"), false -> battleship.Utils.colors("blue"))

    @tailrec
    def fillGridWithOpponentShotsTailRec(g: List[List[String]], s: Set[(Int, Int, Boolean)], x: Int = 0, currentPos: Int = 0): List[List[String]] = {
      if (opponentShots.isEmpty || x == s.size) {
        g
      }
      else {
        val position: (Int, Int, Boolean) = opponentShots.toList(x)
        val newGrid: List[List[String]] = g.updated(position._2, editLine(g(position._2), position, symbs))
        // increase x parameter if we browse all shot's positions
        if (currentPos == opponentShots.size - 1) fillGridWithOpponentShotsTailRec(newGrid, s, x + 1) else fillGridWithOpponentShotsTailRec(newGrid, s, x, currentPos + 1)
      }
    }

    fillGridWithOpponentShotsTailRec(grid, opponentShots)
  }

  /**
    * Update the line of the grid with symb
    *
    * @param line     line to update
    * @param position position where update
    * @param symbs    symbol to add according condition
    * @return new line updated
    */
  def editLine(line: List[String], position: (Int, Int, Boolean), symbs: Map[Boolean, String]): List[String] = {
    line.updated(position._1, symbs(position._3))
  }

  /**
    * Fill the grid with player's boat
    *
    * @return grid filled with player's ships
    */
  def fillGridWithShip: List[List[String]] = {
    val symbs: Map[Boolean, String] = Map(true -> battleship.Utils.colors("red"), false -> battleship.Utils.colors("white"))
    @tailrec
    def fillGridWithShipTailRec(grid: List[List[String]], s: Set[Ship], x: Int = 0, currentPos: Int = 0): List[List[String]] = {
      if (x == s.size) {
        grid
      }
      else {
        val ship: Ship = s.toList(x)
        val positions: Set[(Int, Int, Boolean)] = ship.positions
        val position: (Int, Int, Boolean) = positions.toList(currentPos)
        val newGrid: List[List[String]] = grid.updated(position._2, editLine(grid(position._2), position, symbs))
        // increase x parameter if we browse all ship's positions
        if (currentPos == positions.size - 1) fillGridWithShipTailRec(newGrid, s, x + 1) else fillGridWithShipTailRec(newGrid, s, x, currentPos + 1)
      }
    }
    fillGridWithShipTailRec(generateEmptyGrid, ships)
  }

  /**
    * Generate the grid empty
    *
    * @return empty grid
    */
  def generateEmptyGrid: List[List[String]] = List.fill(size)(List.fill(size)(" "))

  /**
    * Check position validity on the grid
    *
    * @param position tuple X and Y
    * @return true if the position match with the grid
    */
  def checkPosition(position: (Int, Int)): Boolean = position._1 >= 0 && position._2 >= 0 && position._1 < size && position._2 < size

  /**
    * Check ship placement
    * check if ship overpass an other ship
    *
    * @param ship to place on the player's grid
    * @return
    */
  def isValidShip(ship: Ship): Boolean = {
    @tailrec
    def isValidShipTailRec(ship: Ship, ships: Set[Ship]): Boolean = {
      if (ships.isEmpty) {
        true
      } else {
        // if at least one position match with a collection boats then the ship's position will be different
        if (ship.positions.diff(ships.head.positions) != ship.positions) {
          false
        } else {
          isValidShipTailRec(ship, ships.tail)
        }
      }
    }
    isValidShipTailRec(ship, ships)
  }

  /**
    * Add new ship to the grid
    * @param ship the ship to add
    * @return new grid
    */
  def addShip(ship: Ship): Grid = copy(ships = ships + ship)
}

