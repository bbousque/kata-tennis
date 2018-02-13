package fr.bbt.nexity.tennis

import fr.bbt.nexity.tennis.TennisEntities.PointValueEnum._
import fr.bbt.nexity.tennis.TennisEntities.{PFGameScore, PFSetScore}

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
    * Gestion du score pour le set dans les cas spécifiques.
    * Si le set n'est pas gagné, on renvoit la valeur dans le membre de gauche. Sinon, la valeur est à droite.
    */
  val setRule : PFSetScore = {
    case (s1, s2) if s1 == 6 && s2 < 5 || s2 == 6 && s1 < 5  => Right((s1,s2))
    case (s1, s2) if s2 == 6 && s1 == 5 || s2 == 6 && s1 == 5  => Left((s1,s2))
    case (s1, s2) if s1 == 7 && s2 == 5 || s2 == 7 && s1 == 5  => Right((s1,s2))
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
