package chess

case class Opening private(code: String, family: String, name: String, moves: List[String]) {
  override def toString = s"$code $name (${moves.size})"
}

case object Opening {
  def apply(code: String, name: String, moves: String): Opening = {
    val family = name takeWhile { _ != ',' }
    new Opening(code, family, name, moves.split(" ").toList)
  }
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

  def generals: List[(String, String)] = {
    OpeningDB.db.map(_.code).distinct.sorted flatMap { code =>
      val candidates = OpeningDB.db filter (_.code == code)
      candidates find (_.name endsWith "General") orElse
        candidates.sortBy(_.moves.size).headOption map {
          case Opening(a, b, _) => a -> b
        }
    }
  }
}
