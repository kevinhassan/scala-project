package battleship.models

case class Ship(name: String, positions: Set[(Int, Int, Boolean)] = Set(), isSunk: Boolean = false) {

  /**Know all ship's positions not shoot
    * get positions not shoot
    * @return Set[(Int, Int, Boolean)]
    */
  def getPositions: Set[(Int, Int, Boolean)] = positions.filter(!_._3)

  /** Know if the boat is concerned by the opponent's shoot
    * true if the boat is concerned by the shoot
    * @param position position shot by opponent
    * @return boolean
    */
  def isShot(position: (Int, Int)): Boolean = {
    positions.contains(Tuple3(position._1, position._2, false)) || positions.contains(Tuple3(position._1, position._2, true))
  }

  /** Update the ship with the position marked like destroy
    * new ship where the position is shot
    *
    * @param position tuple of X and Y
    * @return Ship
    */
  def destroyPosition(position: (Int, Int)): Ship = {
    val newPos = positions.filter(list => list != Tuple3(position._1, position._2, false)) + Tuple3(position._1, position._2, true)
    val newShip = copy(positions = newPos)
    val nIsSunk = newShip.getPositions.isEmpty
    if (nIsSunk) println("Coulé !")
    newShip.copy(isSunk = nIsSunk)
  }

  /** Know the size of the ship
    * return the size of the ship
    * @return Int
    */
  def size: Int = positions.size

  /**
    * Add position to the ship
    *
    * @param position to add
    * @return new ship
    */
  def addPosition(position: (Int, Int)): Ship = copy(positions = positions + Tuple3(position._1, position._2, false))
}

object Ship {
  val types: Map[String, Int] = Map("carrier" -> 5, "battleship" -> 4, "cruiser" -> 3, "submarine" -> 3, "destroyer" -> 2)

  def apply(name: String, positions: Set[(Int, Int)]): Ship = {
    val pos: Set[(Int, Int, Boolean)] = positions.map(p => Tuple3(p._1, p._2, false))
    new Ship(name, pos)
  }
}