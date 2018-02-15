package fr.bbt.nexity.tennis

import fr.bbt.nexity.tennis.TennisEntities.PointValueEnum._

object TennisEntities {

  type GameScore = (PointValue, PointValue)
  type PFGameScore = PartialFunction[GameScore, GameScore]
  type PFSetScore = PartialFunction[(Int, Int), Either[(Int, Int),(Int, Int)]]

  sealed trait PointValue
  object PointValueEnum {
    case object Zero extends PointValue
    case object Quinze extends PointValue
    case object Trente extends PointValue
    case object Quarante extends PointValue
    case object Deuce extends PointValue
    case object Advantage extends PointValue
    case object Win extends PointValue
  }

  sealed trait Player
  object PlayerEnum {
    case object PlayerOne extends Player
    case object PlayerTwo extends Player

  }

  case class Score(
                    gameScore : (PointValue, PointValue) = (Zero,Zero),
                    currentSet : (Int,Int) = (0,0),
                    sets : Seq[(Int,Int)] = Nil
                  )

}
