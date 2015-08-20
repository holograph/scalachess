package chess
package variant

import chess.Pos._

case object Horde extends Variant(
  id = 8,
  key = "horde",
  name = "Horde",
  shortName = "horde",
  title = "Destroy the horde to win!",
  standardInitialPosition = false) {

  /**
   * In Horde chess white advances against black with a horde of pawns.
   */
  override lazy val pieces: Map[Pos, Piece] = {

    val frontPawns = List(Pos.B5, Pos.C5, Pos.F5, Pos.G5).map { _ -> White.pawn }

    val whitePawnsHoard = frontPawns ++ (for {
      x <- 1 to 8
      y <- 1 to 4
    } yield Pos.at(x, y) map (_ -> White.pawn)).flatten toMap

    val blackPieces = (for (y <- 7 to 8; x <- 1 to 8) yield {
      at(x, y) map { pos =>
        (pos, y match {
          case 8 => Black - backRank(x - 1)
          case 7 => Black.pawn
        })
      }
    }).flatten.toMap

    blackPieces ++ whitePawnsHoard
  }

  override def valid(board: Board, strict: Boolean) =
    board.kingPosOf(White).isEmpty && validSide(board, strict)(Black) && !pawnsOnPromotionRank(board, White)

  /** The game has a special end condition when white manages to capture all of black's pawns */
  override def specialEnd(situation: Situation) =
    situation.board.piecesOf(White).isEmpty

  // is that right?
  override val drawsOnInsufficientMaterial = false

  override def isUnmovedPawn(color: Color, pos: Pos) = {
    color == White && (pos.y == 1 || pos.y == 2)
  } || {
    color == Black && pos.y == 7
  }
}
