package battleship.models

import battleship.Utils
import org.scalatest._

class GridSpec extends FlatSpec with Matchers {
  val s: Int = 10
  val grid: Grid = Grid(s)
  val typeShip: String = "destroyer"

  "The Grid" should "be instantiate" in {
    grid.size shouldBe s
    grid.generateEmptyGrid.size shouldBe s
    grid.generateEmptyGrid.foreach(_.size shouldBe s)
    grid.getAliveShips.isEmpty shouldBe true
  }
  "The Grid" should "check position in the grid" in {
    val position: (Int, Int) = (0, 0)
    grid.checkPosition(position) shouldBe true
  }
  "The Grid" should "check position out of the grid" in {
    val position: (Int, Int) = (s + 1, s + 1)
    grid.checkPosition(position) shouldNot be(true)
  }
  "The Grid" should "convert initial placement" in {
    val limit: Char = ('A'.toInt + s - 1).toChar
    val positions: Set[(Int, Int)] = ('A' to limit).toSet.map { letter: Char => Utils.convertPosition(letter, 1) }
    positions.size shouldBe s
  }
  "The Grid" should "correctly place the ship" in {
    val rightShip: Ship = Ship(typeShip, Set((0, 0), (1, 1)))
    grid.isValidShip(rightShip) shouldBe true
  }
  "The Grid" should "place the ship on an other" in {
    val ship: Ship = Ship(typeShip, Set((0, 0), (0, 1)))
    val newGrid: Grid = grid.addShip(ship)
    val wrongShip: Ship = Ship(typeShip, Set((1, 0), (0, 1)))
    newGrid.isValidShip(wrongShip) shouldNot be(true)
  }
  "The Grid" should "be fill" in {
    val ship1: Ship = Ship(typeShip, Set((0, 0), (0, 1)))
    val ship2: Ship = Ship(typeShip, Set((1, 0), (1, 1)))
    val newGrid1: Grid = grid.addShip(ship1)
    val newGrid2: Grid = newGrid1.addShip(ship2)
    newGrid1 shouldNot equal(newGrid2)
    val matrix1: List[List[String]] = newGrid1.fillGridWithShip
    val matrix2: List[List[String]] = newGrid2.fillGridWithShip
    matrix1 shouldNot equal(newGrid1.generateEmptyGrid)
    matrix2 shouldNot equal(newGrid2.generateEmptyGrid)
    matrix1 shouldNot equal(matrix2)
    matrix2.head.head shouldNot equal(" ")
    matrix2.head(1) shouldNot equal(" ")
    matrix2(1).head shouldNot equal(" ")
    matrix2(1)(1) shouldNot equal(" ")
  }
  "The Grid" should "have ship sunk" in {
    val newGrid: Grid = grid.addShip(Ship("destroyer", Set((0, 0), (0, 1))))
    newGrid.getAliveShips.isEmpty shouldBe false
    val grid2: Grid = Grid(s, Set(Ship("destroyer", Set((0, 0, true), (0, 1, true)), true)))
    grid2.getAliveShips.isEmpty shouldBe true
  }

}
