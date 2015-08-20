package chess

case class Piece(color: Color, role: Role) {

  def is(c: Color) = c == color
  def is(r: Role) = r == role
  def isNot(r: Role) = r != role

  def oneOf(rs: Set[Role]) = rs(role)

  def isMinor = oneOf(Set(Knight, Bishop))
  def isMajor = oneOf(Set(Queen, Rook))

  def forsyth: Char = if (color == White) role.forsythUpper else role.forsyth

  // attackable positions assuming empty board
  def eyes(from: Pos, to: Pos): Boolean = role match {
    case King   => from touches to
    case Queen  => (from onSameLineAs to) || (from onSameDiagonalAs to)
    case Rook   => from onSameLineAs to
    case Bishop => from onSameDiagonalAs to
    case Knight =>
      val xd = from columnDistance to
      val yd = from rowDistance to
      (xd == 1 && yd == 2) || (xd == 2 && yd == 1)
    case Pawn => Piece.pawnEyes(color, from, to)
  }

  // movable positions assuming empty board
  def eyesMovable(from: Pos, to: Pos): Boolean =
    if (role == Pawn) Piece.pawnEyes(color, from, to) || {
      (from onSameColumnAs to) && {
        val dy = to.y - from.y
        if (color.white) (dy == 1 || (from.y <= 2 && dy == 2))
        else (dy == -1 || (from.y >= 7 && dy == -2))
      }
    }
    else eyes(from, to)

  override def toString = (color + "-" + role).toLowerCase
}

object Piece {

  private def pawnEyes(color: Color, from: Pos, to: Pos) =
    (from columnDistance to) == 1 && (to.y - from.y) == {
      if (color.white) 1 else -1
    }
}
