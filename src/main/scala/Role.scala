package chess

import Directions._

sealed trait Role {
  val forsyth: Char
  lazy val forsythUpper: Char = forsyth.toUpper
  lazy val pgn: Char = forsythUpper
  lazy val name = toString.toLowerCase
  val attacker: Boolean = true
  val projection: Boolean
  val dirs: Directions
  def dir(from: Pos, to: Pos): Option[Direction]
}

sealed trait PromotableRole extends Role

/**
 * Promotable in antichess.
 */
case object King extends PromotableRole {
  val forsyth = 'k'
  val dirs: Directions = Queen.dirs
  def dir(from: Pos, to: Pos) = None
  override val attacker = false
  val projection = false
}

case object Queen extends PromotableRole {
  val forsyth = 'q'
  val dirs: Directions = cardinals ::: diagonals
  def dir(from: Pos, to: Pos) = Rook.dir(from, to) orElse Bishop.dir(from, to)
  val projection = true
}

case object Rook extends PromotableRole {
  val forsyth = 'r'
  val dirs: Directions = cardinals
  def dir(from: Pos, to: Pos) = if (to ?| from) Some(
    if (to ?^ from) up else down
  )
  else if (to ?- from) Some(
    if (to ?< from) left else right
  )
  else None
  val projection = true
}

case object Bishop extends PromotableRole {
  val forsyth = 'b'
  val dirs: Directions = diagonals
  def dir(from: Pos, to: Pos) = if (to onSameDiagonal from) Some(
    if (to ?^ from) {
      if (to ?< from) upLeft else upRight
    }
    else {
      if (to ?< from) downLeft else downRight
    }
  )
  else None
  val projection = true
}
case object Knight extends PromotableRole {
  val forsyth = 'n'
  val dirs: Directions = List(
    up andThen upLeft,
    up andThen upRight,
    left andThen upLeft,
    left andThen downLeft,
    right andThen upRight,
    right andThen downRight,
    down andThen downLeft,
    down andThen downRight)
  def dir(from: Pos, to: Pos) = None
  val projection = false
}
case object Pawn extends Role {
  val forsyth = 'p'
  val dirs: Directions = Nil
  def dir(from: Pos, to: Pos) = None
  val projection = false
}

object Role {

  val all: List[Role] = List(King, Queen, Rook, Bishop, Knight, Pawn)
  val allPromotable: List[PromotableRole] = List(Queen, Rook, Bishop, Knight, King)
  val allByForsyth: Map[Char, Role] = all map { r => (r.forsyth, r) } toMap
  val allByPgn: Map[Char, Role] = all map { r => (r.pgn, r) } toMap
  val allPromotableByName: Map[String, PromotableRole] =
    allPromotable map { r => (r.toString, r) } toMap
  val allPromotableByForsyth: Map[Char, PromotableRole] =
    allPromotable map { r => (r.forsyth, r) } toMap
  val allPromotableByPgn: Map[Char, PromotableRole] =
    allPromotable map { r => (r.pgn, r) } toMap

  def forsyth(c: Char): Option[Role] = allByForsyth get c

  def promotable(c: Char): Option[PromotableRole] =
    allPromotableByForsyth get c

  def promotable(name: String): Option[PromotableRole] =
    allPromotableByName get name.capitalize

  def promotable(name: Option[String]): Option[PromotableRole] =
    name flatMap promotable
}
