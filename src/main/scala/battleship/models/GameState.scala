package battleship.models

/**
  * GameState class store the game state
  *
  * @param players  collection of players playing the battleship game
  * @param launcher the player who initiate the game
  */
case class GameState(players: Set[Player], launcher: Player) {
  /**
    * Check if the party is finished
    *
    * @return true if the opponent player has no ship
    */
  def isFinish: Boolean = !getOpponentPlayer.hasShip

  /**
    * Get the opponent player
    * Opponent player is the last of the collection of players
    *
    * @return Player opponent player
    */
  def getOpponentPlayer: Player = players.last

  /**
    * Change the active player for the next round
    * switch opponentPlayer with activePlayer
    *
    * @return GameState players collections is permuted
    */
  def nextRound: GameState = copy(players = Set(getOpponentPlayer, getActivePlayer))

  /**
    * Get the active player
    * Active player is the head of the collection of players
    *
    * @return Player active player
    */
  def getActivePlayer: Player = players.head
}
