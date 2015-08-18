package format

import chess.{Openings, OpeningDB}
import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification

/**
 * Created by tomerga on 8/18/15.
 */
class OpeningIndexTest extends Specification with Matchers {

  // Original code --

  lazy val codeFamily: Map[String, String] = {
    OpeningDB.db.foldLeft(Map[String, List[String]]()) {
      case (acc, opening) =>
        acc + (opening.code -> (opening.family :: acc.getOrElse(opening.code, Nil)))
    }.flatMap {
      case (code, families) =>
        families
          .groupBy(identity)
          .mapValues(_.size)
          .toList.sortBy(-_._2)
          .headOption
          .map(_._1)
          .map(code -> _)
    }
  }

  lazy val familyFirstMove: Map[String, String] = OpeningDB.db.foldLeft(Map[String, String]()) {
    case (acc, opening) if acc contains opening.family => acc
    case (acc, opening) => opening.moves.headOption.fold(acc) { firstMove =>
      acc + (opening.family -> firstMove)
    }
  }

  lazy val familyMoveList: Map[String, List[String]] = OpeningDB.db.foldLeft(Map[String, List[String]]()) {
    case (acc, opening) => acc.get(opening.family).filter(_.size < opening.moves.size) match {
      case None => acc + (opening.family -> opening.moves)
      case _    => acc
    }
  }

  lazy val generals: List[(String, String)] = {
    OpeningDB.db.map(_.code).distinct.sorted flatMap { code =>
      val candidates = OpeningDB.db filter (_.code == code)
      candidates find (_.name endsWith "General") orElse
        candidates.sortBy(_.moves.size).headOption map { opening => opening.code -> opening.name }
    }
  }


  // Verification --

  "codeFamily index" should {
    "survive refactoring" in {
      codeFamily should_=== Openings.codeFamily
    }
  }

  "familyFirstMove index" should {
    "survive refactoring" in {
      familyFirstMove should_=== Openings.familyFirstMove
    }
  }

  "familyMoveList index" should {
    "survive refactoring" in {
      familyMoveList should_=== Openings.familyMoveList
    }
  }

  "generals index" should {
    "survive refactoring" in {
      generals should_=== Openings.generals
    }
  }
}
