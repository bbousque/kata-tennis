package fr.bbt.nexity.tennis

import fr.bbt.nexity.tennis.TennisEntities.PointValueEnum._
import fr.bbt.nexity.tennis.TennisEntities.{GameScore, PFGameScore, PFSetScore}

/**
  * Règles de gestion des points pour le jeu et le set
  */
trait TennisRules {

  /**
    * Incrément des points d'un jeu
    * @param point  La valeur du point actuel
    * @return       La valeur actualisée
    */
  def increment(point : PointValue): PointValue = point match {
    case Deuce => Advantage
    case Quarante | Advantage => Win
    case _ => classicGame(classicGame.indexOf(point)+1)
  }

  /**
    * Cas de la règle DEUCE
    */
  val deuceRule : PFGameScore = {
    case (Advantage, Advantage) | (Quarante, Quarante)  => (Deuce, Deuce)
  }

  /**
    * Cas de gestion pour la règle normal. On ne modifie pas la valeur.
    */
  val normalRule : PFGameScore = {
    case s  => s
  }

  /**
    * Constitution de la méthode globale de gestion des règles du jeu
    */
  val gameRules: PFGameScore = deuceRule.orElse(normalRule)

  /**
    * Règle déterminant si le set est gagnant
    */
  object winSetRule {
    def unapply(score: (Int,Int)): Boolean = {
      val (s1, s2) = score

      (s1 == 6 && s2 < 5 || s2 == 6 && s1 < 5) || (s1 == 7 && s2 == 5 || s2 == 7 && s1 == 5)
    }
  }

  /**
    * Règle déterminant si le set doit comporter un jeu supplémentaire
    */
  object extraGameSetRule {
    def unapply(score: (Int,Int)): Boolean = {
      val (s1, s2) = score

      s2 == 6 && s1 == 5 || s2 == 6 && s1 == 5
    }
  }

  /**
    * Gestion du score pour le set dans les cas spécifiques.
    * Si le set n'est pas gagné, on renvoit la valeur dans le membre de gauche. Sinon, la valeur est à droite.
    */
  val setRule : PFSetScore = {
    case score @ winSetRule() => Right(score)
    case score @ extraGameSetRule() => Left(score)
  }

  /**
    * Cas normal non spécifique. Le set continue avec la valeur à gauche
    */
  val normalSetRule : PFSetScore = {
    case s  => Left(s)
  }

  /**
    * Règles complètes pour tous les cas
    */
  val setRules: PFSetScore = setRule.orElse(normalSetRule)

}
