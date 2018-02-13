package fr.bbt.nexity.tennis

import fr.bbt.nexity.tennis.TennisEntities.PointValueEnum.{PointValue, Zero}

/**
  * Entités applicatives
  */
object TennisEntities {

  /**
    * Types utilisés dans l'application
    */
  type GameScore = (PointValue, PointValue)
  type PFGameScore = PartialFunction[GameScore, GameScore]
  type PFSetScore = PartialFunction[(Int, Int), Either[(Int, Int),(Int, Int)]]

  /**
    * Enumération des points possibles dans un jeu
    */
  object PointValueEnum {
    sealed trait PointValue
    case object Zero extends PointValue
    case object Quinze extends PointValue
    case object Trente extends PointValue
    case object Quarante extends PointValue
    case object Deuce extends PointValue
    case object Advantage extends PointValue
    case object Win extends PointValue

    val classicGame = Seq(Zero, Quinze, Trente, Quarante)
  }

  /**
    * Sélection du joueur
    */
  object PlayerEnum {
    sealed trait Player

    case object PlayerOne extends Player
    case object PlayerTwo extends Player

  }
    /**
    * Classe représentant le score actuel du match
    * @param gameScore    Le score du jeu en cours
    * @param currentSet   Le score du set en cours
    * @param sets         Liste des sets terminés
    */
  case class Score(
                    gameScore : (PointValue, PointValue) = (Zero,Zero),
                    currentSet : (Int,Int) = (0,0),
                    sets : Seq[(Int,Int)] = Nil
                  )

}
