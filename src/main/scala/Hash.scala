package chess

import java.security.MessageDigest

import chess.format.Forsyth

object Hash {

  private[chess] val size = 3

  private def apply(str: String): PositionHash =
    MessageDigest getInstance "MD5" digest (str getBytes "UTF-8") take size

  def apply(actors: Iterable[Actor], color: Color): PositionHash = apply {
    actors.map { a =>
      s"${Forsyth.of(a.piece)}${a.pos.key}"
    }.mkString + color.letter
  }
}
