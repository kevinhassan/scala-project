package battleship.models

import org.scalatest._

class ShipSpec extends FlatSpec with Matchers {
  "The ship `destroyer`" should "be instantiate" in {
    val shipName: String = "destroyer"
    val shipSize: Option[Int] = Ship.types.get(shipName)
    val ship: Ship = Ship(shipName, Set((0, 0, false), (1, 1, false)))
    ship.name shouldBe shipName
    ship.size shouldBe shipSize.get
    ship.isSunk shouldBe false
    ship.getPositions.size shouldBe shipSize.get
  }
  "The ship `destroyer`" should "be shot once" in {
    val shipName: String = "destroyer"
    val shipSize: Option[Int] = Ship.types.get(shipName)
    val ship: Ship = Ship(shipName, Set((0, 0, false), (1, 1, false)))
    ship.isShot((0, 0)) shouldBe true
    val newShip: Ship = ship.destroyPosition((0, 0))
    newShip.getPositions.size shouldBe (shipSize.get - 1)
    newShip.isSunk shouldBe false
    // if the position was already shot no need to return true
    newShip.isShot((0, 0)) shouldBe true
  }
  "The ship `destroyer`" should "be sunk" in {
    val shipName: String = "destroyer"
    val shipSize: Option[Int] = Ship.types.get(shipName)
    val ship: Ship = Ship(shipName, Set((0, 0, false), (1, 1, false)))
    ship.isShot((0, 0)) shouldBe true
    val newShip1: Ship = ship.destroyPosition((0, 0))
    newShip1.isSunk shouldBe false
    newShip1.getPositions.size shouldBe (shipSize.get - 1)
    newShip1.isShot((1, 1)) shouldBe true
    val newShip2: Ship = newShip1.destroyPosition((1, 1))
    newShip2.isSunk shouldBe true
    println(newShip2.isSunk)
    newShip2.getPositions.size shouldBe 0
  }
}
