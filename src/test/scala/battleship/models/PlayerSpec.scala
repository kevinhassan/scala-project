package battleship.models

import org.scalatest._

class PlayerSpec extends FlatSpec with Matchers{
  val player1: Player = Player("player1", Grid(10), isHuman = true)

  "The player" should "be instantiate" in {
    player1.username shouldBe "player1"
    player1.score shouldBe 0
    player1.shipSize shouldBe 0
    player1.hasShip shouldBe false
    player1.getAliveShips shouldBe Set()
  }
  "The player" should "create ship horizontally" in {
    val shipType: String = "destroyer"
    val position: (Int, Int) = (0, 0)
    val direction: Char = 'h'
    val positions: Set[(Int, Int)] = Set(position, (position._1 + 1, position._2))
    val newShip: Option[Ship] = player1.createShip(shipType, position, direction)
    newShip.get shouldBe Ship(shipType, positions)
    val p: Player = player1.addShip(newShip.get)
    p.hasShip shouldBe true
    p.getAliveShips.size should be > 0
  }
  "The player" should "reset player" in {
    val shipType: String = "destroyer"
    val newShip: Ship = Ship(shipType, Set((0, 0), (1, 1)))
    val p: Player = player1.addShip(newShip)
    p.hasShip shouldBe true
    p.resetPlayer.hasShip shouldBe false
    p.copy(score = p.score + 1).resetPlayer.score shouldBe 1
    p.resetPlayer.username shouldBe player1.username
  }
  "The player" should "create ship verticaly" in {
    val shipType: String = "destroyer"
    val position: (Int, Int) = (0, 0)
    val direction: Char = 'v'
    val positions: Set[(Int, Int)] = Set(position, (position._1, position._2 + 1))
    val newShip: Option[Ship] = player1.createShip(shipType, position, direction)
    newShip.get shouldBe Ship(shipType, positions)
    val p = player1.addShip(newShip.get)
    p.hasShip shouldBe true
    p.getAliveShips.size should be > 0
  }
  "The player" should "create 2 ship verticaly and horizontaly" in {
    val shipType: String = "destroyer"
    val position1: (Int, Int) = (0, 0)
    val positions1: Set[(Int, Int)] = Set(position1, (position1._1, position1._2 + 1))
    val newShip1: Option[Ship] = player1.createShip(shipType, position1, 'v')
    newShip1.get shouldBe Ship(shipType, positions1)
    val p = player1.addShip(newShip1.get)

    val position2: (Int, Int) = (1, 0)
    val positions2: Set[(Int, Int)] = Set(position2, (position2._1, position2._2 + 1))
    val newShip2: Option[Ship] = p.createShip(shipType, position2, 'v')
    newShip2.get shouldBe Ship(shipType, positions2)
    p.addShip(newShip2.get).shipSize shouldBe 2
    p.addShip(newShip2.get).getAliveShips.size shouldBe 2
  }
  "The player" should "create 2 ship one over the over" in {
    val shipType: String = "destroyer"
    val position1: (Int, Int) = (0, 0)
    val positions1: Set[(Int, Int)] = Set(position1, (position1._1, position1._2 + 1))
    val newShip1: Option[Ship] = player1.createShip(shipType, position1, 'v')
    newShip1.get shouldBe Ship(shipType, positions1)
    val p = player1.addShip(newShip1.get)

    val position2: (Int, Int) = (0, 0)
    val newShip2: Option[Ship] = p.createShip(shipType, position2, 'v')
    newShip2 shouldBe None
  }
  "The player" should "shot a position on the grid" in {
    player1.makeAShot((0, 0, false)).grid shouldNot be(player1.grid)
  }
}
