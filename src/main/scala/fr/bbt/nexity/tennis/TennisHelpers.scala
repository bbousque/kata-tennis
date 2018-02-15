package fr.bbt.nexity.tennis

import fr.bbt.nexity.tennis.TennisEntities.PlayerEnum.{PlayerOne, PlayerTwo}
import fr.bbt.nexity.tennis.TennisEntities.PointValueEnum.{Win, Zero}
import fr.bbt.nexity.tennis.TennisEntities.{GameScore, Player, PointValue, Score}

object TennisHelpers {

  implicit class ScoreHelper(score: Score) extends TennisRules {

    def addPoint[T](addFn : T => T) = (player : Player, score : (T,T)) => {
      player match {
        case PlayerOne => (addFn(score._1), score._2)
        case PlayerTwo => (score._1, addFn(score._2))
      }
    }: (T, T)

    def addGamePoint = addPoint[PointValue](increment)
    def addSetPoint = addPoint[Int]( point => point + 1)

    /**
      * Gain d'un point pour le joueur précisé
      * @param player   L'entrée de l'énumration du joureur qui marque
      * @return         Le score actualisé avec le gain du point pour le jeu
      */
    def winPoint(player: Player): Score = score.copy(
      gameScore = gameRules(addGamePoint(player, score.gameScore))
    )

    /**
      * Mise à jour du score du set
      */
    def updateSetScore(): Score = {
      /* Si le jeu est gagnant, on met à jour le score du set */
      val newScore = score.gameScore match {
        case (Win, _) => score.copy(
          currentSet = (score.currentSet._1 + 1, score.currentSet._2),
          gameScore = (Zero, Zero)
        )
        case (_, Win) => score.copy(
          currentSet = (score.currentSet._1, score.currentSet._2 + 1),
          gameScore = (Zero, Zero)
        )
        case _ => score
      }

      /* On applique les règles de gestion du set pour savoir si il est toujours en cours (valeur de gauche) ou remporté (valeur de droite) */
      setRules(newScore.currentSet) match {
        case Left(s) => newScore.copy(currentSet = s)
        case Right(s) => newScore.copy(sets = score.sets :+ s, currentSet = (0, 0))
      }
    }

    /**
      * On applique les méthodes applicatives pour le gain du point du joueur précisé.
      * On récupère le nouveau score actualisé.
      */
    def updateScore(player: Player): Score = {
      winPoint(player)
        .updateSetScore()
    }
  }
}
