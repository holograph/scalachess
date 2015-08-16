package chess

case class Opening private(code: String, name: String, moves: Seq[String]) {
  val family = name takeWhile { _ != ',' }

  def fullName = s"$code $name"    // TODO refactor out

  lazy val moveList = moves.toList   // TODO refactor out

  def firstMove = moves.headOption    // TODO refactor out

  def size = moves.size   // TODO refactor out

  def familyName = family  // TODO refactor out

  override def toString = s"$fullName ($size)"
}

case object Opening {
  def apply(code: String, name: String, moves: String): Opening =
    new Opening(code, name, moves.split(" "))
}

object Openings {   // TODO fold into Opening
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
    case (acc, opening) if (acc contains opening.familyName) => acc
    case (acc, opening) => opening.firstMove.fold(acc) { firstMove =>
      acc + (opening.familyName -> firstMove)
    }
  }

  lazy val familyMoveList: Map[String, List[String]] = OpeningDB.db.foldLeft(Map[String, List[String]]()) {
    case (acc, opening) => acc.get(opening.familyName).filter(_.size < opening.size) match {
      case None => acc + (opening.familyName -> opening.moveList)
      case _    => acc
    }
  }

  def generals: List[(String, String)] = {
    OpeningDB.db.map(_.code).distinct.sorted flatMap { code =>
      val candidates = OpeningDB.db filter (_.code == code)
      candidates find (_.name endsWith "General") orElse
        candidates.sortBy(_.size).headOption map {
          case Opening(a, b, _) => a -> b
        }
    }
  }
}
