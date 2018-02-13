package fr.bbt.nexity.tennis

import fr.bbt.nexity.tennis.PointValueEnum.{PointValue, Zero}

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

case class Player(name : String, gamePoint : PointValueEnum.PointValue)

case class Match(p1 : Player, p2 : Player)

case class Score(gameScore : (PointValue, PointValue) = (Zero,Zero), currentSet : (Int,Int) = (0,0), sets : Seq[(Int,Int)] = Nil)

trait Game {
  import PointValueEnum._

  type GameScore = (PointValue, PointValue)
  type PFGameScore = PartialFunction[GameScore, GameScore]

  def increment(point : PointValue): PointValue = point match {
    case Deuce => Advantage
    case Quarante | Advantage => Win
    case _ => classicGame(classicGame.indexOf(point)+1)
  }

  val deuceRule : PFGameScore = {
    case (Advantage, Advantage) | (Quarante, Quarante)  => (Deuce, Deuce)
  }

  val normalRule : PFGameScore = {
    case s  => s
  }

  val gameRules: PFGameScore = deuceRule.orElse(normalRule)
}

trait Set {

  type PFSetScore = PartialFunction[(Int, Int), Either[(Int, Int),(Int, Int)]]

  val setRule : PFSetScore = {
    case (s1, s2) if s1 == 6 && s2 < 5 || s2 == 6 && s1 < 5  => Right((s1,s2))
    case (s1, s2) if s2 == 6 && s1 == 5 || s2 == 6 && s1 == 5  => Left((s1,s2))
    case (s1, s2) if s2 == 7 && s1 == 5 || s2 == 7 && s1 == 5  => Right((s1,s2))
  }

  val normalSetRule : PFSetScore = {
    case s  => Left(s)
  }

  val setRules: PFSetScore = setRule.orElse(normalSetRule)


}

/**
  * Created by BBOUSQUE on 13/02/2018.
  */
object Match extends Game with Set {
  import PointValueEnum._

  def gameScore(indexPlayer : Int, score : Score) = score.copy(gameScore = gameRules(indexPlayer match {
    case 1 => (increment(score.gameScore._1), score.gameScore._2)
    case 2 => (score.gameScore._1, increment(score.gameScore._2))
  }))

  def setScore(score : Score) = {
    val newScore = score.gameScore match {
      case (Win, _) => score.copy(
        currentSet = (score.currentSet._1 + 1, score.currentSet._2),
        gameScore = (Zero,Zero)
      )
      case (_, Win) => score.copy(
        currentSet = (score.currentSet._1 + 1, score.currentSet._2),
        gameScore = (Zero,Zero)
      )
      case _ => score
    }

    setRules(newScore.currentSet) match {
      case Left(s) => newScore.copy(currentSet = s)
      case Right(s) => newScore.copy(sets = score.sets :+ s, currentSet = (0,0))
    }
  }

  def updateScore(indexPlayer : Int, score : Score): Score = {
    setScore(
      gameScore(indexPlayer, score)
    )
  }

}
