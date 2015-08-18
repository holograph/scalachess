package chess

case class Opening(code: String, family: String, name: String, moves: List[String]) {
  override def toString = s"$code $name (${moves.size})"
}

case object Opening {
  def parse(code: String, name: String, moves: String): Opening = {
    val family = name takeWhile { _ != ',' }
    new Opening(code, family, name, moves.split(" ").toList)
  }
}

object Openings {   // TODO fold into OpeningDB

  trait Histogram[T] {
    def cardinalities: Map[T, Int]
    def mostFrequent: T
    def leastFrequent: T
  }

  implicit class ToMultimapEnrichment[T](coll: Iterable[T]) {
    def toMultimap[K, V](mapKey: T => K, mapElement: T => V): Map[K, Seq[V]] = {
      coll.foldLeft(Map.empty[K, List[V]]) {
        case (mmap, e) =>
          val key = mapKey(e)
          mmap + (key -> (mapElement(e) :: mmap.getOrElse(key, Nil)))
      }
    }

    def histogram: Histogram[T] = new Histogram[T] {
      val cardinalities = coll.groupBy(identity).mapValues(_.size)
      def mostFrequent = cardinalities.maxBy(_._2)._1
      def leastFrequent = cardinalities.minBy(_._2)._1
    }
  }

  lazy val codeFamily: Map[String, String] = {
    val codeFamilies = OpeningDB.db.toMultimap(_.code, _.family)

    def selectBestFamily(families: Seq[String]): String =
      families.histogram.mostFrequent

    codeFamilies.mapValues(selectBestFamily)
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
        candidates.sortBy(_.moves.size).headOption map { opening => opening.code -> opening.name }
    }
  }
}
