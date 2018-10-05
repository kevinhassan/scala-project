package battleship.models

case class GameState(players: Set[Player], launcher: Player) {
  /**
    *
    * @return
    */
  def isFinish: Boolean = !getOpponentPlayer.hasShip

  /**Get the active player
    * return the active player
    * @return Player
    */
  def getActivePlayer: Player = players.head

  /**Get the opponent player
    * return the opponent player
    * @return Player
    */
  def getOpponentPlayer: Player = players.last

  /**Change the active player for the next round
    * switch opponentPlayer with activePlayer
    * @return Set[Player]
    */
  def nextRound: GameState = copy(players = Set(getOpponentPlayer, getActivePlayer))
}
