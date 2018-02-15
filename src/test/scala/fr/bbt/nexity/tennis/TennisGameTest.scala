package fr.bbt.nexity.tennis

import fr.bbt.nexity.tennis.TennisEntities.PlayerEnum.{PlayerOne, PlayerTwo}
import fr.bbt.nexity.tennis.TennisEntities.PointValueEnum.{Quinze, Zero}
import fr.bbt.nexity.tennis.TennisEntities.Score
import org.scalatest.FunSuite

/**
  * Tests du jeu de tennis
  */
class TennisGameTest extends FunSuite {

  /**
    * Simulation de l'enchaînement de points
    */
  test("Play Match") {
    import TennisHelpers._
    val coups = List(PlayerOne,PlayerOne,PlayerTwo,PlayerTwo,PlayerOne,PlayerTwo,PlayerOne,PlayerTwo,PlayerOne,PlayerOne,PlayerOne)

    val scores = coups.foldLeft(List(Score())) {
      (agg,a) => agg :+ agg.last.updateScore(a)
    }

    scores.foreach(println)

    assert(
      scores.last equals Score((Quinze,Zero),(1,0),List())
    )
  }
}
