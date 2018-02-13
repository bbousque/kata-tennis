package fr.bbt.nexity.tennis

import fr.bbt.nexity.tennis.TennisEntities.PointValueEnum._
import org.scalatest.FunSuite

/**
  * Test des règles du jeu
  */
class TennisRulesTest extends FunSuite with TennisRules {

  /**
    * On teste la progression du score pour un jeu normal
    */
  test("Increment Score Game") {
    val resultGame = for {
      s <- classicGame.take(4)
    } yield increment(s)

    assert(
      resultGame equals List(Quinze, Trente, Quarante, Win)
    )
  }

  /**
    * On teste la règle DEUCE dans le cas d'un jeu disputé
    */
  test("DEUCE Rule") {

    val case1 = gameRules((increment(Trente), Quarante)) equals (Deuce,Deuce)
    val case2 = gameRules((increment(Deuce), Deuce)) equals (Advantage,Deuce)
    val case3 = gameRules((increment(Advantage), Deuce)) equals (Win,Deuce)
    val case4 = gameRules((increment(Deuce), Advantage)) equals (Deuce,Deuce)

    assert(
      case1 && case2 && case3 && case4
    )
  }

  /**
    * On teste l'application des règles pour un set disputé.
    * Dans le cas où la valeur est à droite, le set est gagné, sinon le set continue.
    */
  test("Set Rules") {
    assert(
      setRules((6,4)).isRight &&
        setRules((6,5)).isLeft &&
        setRules((7,5)).isRight
    )
  }
}
