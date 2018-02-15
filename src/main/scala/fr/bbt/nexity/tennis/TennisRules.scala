package fr.bbt.nexity.tennis

import fr.bbt.nexity.tennis.TennisEntities.PointValueEnum._
import fr.bbt.nexity.tennis.TennisEntities._

trait TennisRules {

  /**
    * Incrémente les points d'un jeu
    */
  def increment(point : PointValue): PointValue = point match {
    case Deuce => Advantage
    case Quarante | Advantage => Win
    case Zero => Quinze
    case Quinze => Trente
    case Trente => Quarante
  }

  /**
    * Cas de la règle DEUCE
    */
  val deuceRule : PFGameScore = {
    case (Advantage, Advantage) | (Quarante, Quarante)  => (Deuce, Deuce)
  }

  val normalRule : PFGameScore = {
    case s  => s
  }

  val gameRules: PFGameScore = deuceRule.orElse(normalRule)

  /**
    * Règle déterminant si le set est gagnant
    */
  object winSetRule {
    def unapply(score: (Int,Int)): Boolean = {
      score match {
        case (6, p) if p < 5 => true
        case (p, 6) if p < 5 => true
        case (7,5) | (5,7) => true
        case _ => false
      }
    }
  }

  /**
    * Règle déterminant si le set doit comporter un jeu supplémentaire
    */
  object extraGameSetRule {
    def unapply(score: (Int,Int)): Boolean = {
      score match {
        case (6,5) | (5,6) => true
        case _ => false
      }
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
