package battleship.models

import scala.annotation.tailrec
import scala.collection.immutable.Set

/**
  * A player who play battleship game
  *
  * @constructor create a new player with ships, shots and score
  * @param username player's username
  * @param grid     player's grid
  * @param score    score of the player during the game
  */
case class Player(username: String, grid: Grid, isHuman: Boolean, score: Int = 0) {
  /** Know if the player has ship
    * True if at least one ship for the player
    * @return boolean
    */
  def hasShip: Boolean = getAliveShips.nonEmpty

  /** Get all ships alive on the player's grid
    * Collection of ships not sink
    *
    * @return Set[Ship]
    */
  def getAliveShips: Set[Ship] = grid.getAliveShips

  /** Shot grid position
    * Add new shot position to the player's grid
    *
    * @param position position targeted by the active player and result of the shot
    * @return new opponent state
    */
  def makeAShot(position: (Int, Int, Boolean)): Player = {
    copy(grid = grid.makeAShot(position))
  }

  /**
    * Take a shot from the opponent
    * @param position where the opponent shot
    * @return
    */
  def takeAShot(position: (Int, Int)): Player = {
    copy(grid = grid.takeAShot(position))
  }

  /** Create new ship to the player
    *
    * @param position tuple X,Y
    * @return new boats or error
    */
  def createShip(typeName: String, position: (Int, Int), direction: Char): Option[Ship] = {
    @tailrec
    def createShipTailRec(position: (Int, Int), direction: Char, ship: Ship): Option[Ship] = {
      // Check if the position is correct
      if (ship.size == Ship.types(ship.name)) {
        Some(ship).filter(grid.isValidShip)
      }
      else {
        if (!grid.checkPosition(position)) {
          None
        }
        else {
          direction match {
            case 'h' => createShipTailRec((position._1 + 1, position._2), direction, ship.addPosition(position))
            case 'v' => createShipTailRec((position._1, position._2 + 1), direction, ship.addPosition(position))
          }
        }
      }
    }

    createShipTailRec(position, direction, Ship(typeName))
  }

  /**Add the ship to the player
    *
    * @param ship new ship to add
    * @return the player with a new grid
    */
  def addShip(ship: Ship): Player = {
    val newGrid = grid.addShip(ship)
    copy(grid = newGrid)
  }
  /**Reset ships and shots of the player
    * when the player launch a new game his ships and shots are reset
    * @return
    */
  def resetPlayer: Player = copy(grid = Grid(grid.size))

  /**Get fleet size
    * Return the number of ships' collection on the grid
    *
    * @return Int number of ship
    */
  def shipSize: Int = if (grid.ships.isEmpty) 0 else grid.ships.size
}
