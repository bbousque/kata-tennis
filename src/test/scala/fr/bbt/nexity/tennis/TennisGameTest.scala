package fr.bbt.nexity.tennis

import org.scalatest.FunSuite
import PointValueEnum._
import Match._

/**
  * Created by BBOUSQUE on 13/02/2018.
  */
class TennisGameTest extends FunSuite {

  test("Increment Score Game") {
    val resultGame = for {
      s <- classicGame.take(4)
    } yield increment(s)

    assert(
      resultGame equals List(Quinze, Trente, Quarante, Win)
    )
  }

  test("DEUCE Rule") {
/*    val case1 = updateScore((Trente, Quarante)) equals (Deuce,Deuce)
    val case2 = updateScore((Deuce, Deuce)) equals (Advantage,Deuce)
    val case3 = updateScore((Advantage, Deuce)) equals (Win,Deuce)
    val case4 = updateScore((Deuce, Advantage)) equals (Deuce,Deuce)

    assert(
      case1 && case2 && case3 && case4
    )
  }

  test("Set Rule") {
    println(setRules((6,4)))

    assert(
      true
    ) */
  }


  test("Play Match") {
    val coups = List(1,1,2,2,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

    coups.foldLeft(List(Score())) { (agg,a) => agg :+ updateScore(a, agg.last) }.foreach(println)

    assert(
      true
    )
  }
}
