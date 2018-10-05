import battleship.models.{Grid, Player}
import org.scalatest._

class Utils extends FlatSpec with Matchers {
  "The grid printer" should "print grid ships" in {
    val player1: Player = Player("p1", Grid(10), isHuman = true)
    val player2: Player = Player("p2", Grid(10), isHuman = true)
    val newPlayer1: Player = player1.addShip(player1.createShip("destroyer",(0,0), 'v').get)
    val pos: (Int, Int, Boolean) = (0, 0, true)
    val newPlayer2: Player = player2.makeAShot(pos)
    val newPlayer12: Player = newPlayer1.takeAShot((pos._1, pos._2))
  }
}


