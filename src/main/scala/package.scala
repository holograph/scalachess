import ornicar.scalalib

package object chess

    extends scalalib.Validation
    with scalalib.Common
    with scalalib.OrnicarNonEmptyList
    with scalalib.OrnicarMonoid.Instances

    with scalaz.syntax.std.ToBooleanOps

    with scalaz.std.OptionFunctions
    with scalaz.syntax.std.ToOptionOps
    with scalaz.syntax.std.ToOptionIdOps

    with scalaz.std.ListInstances
    with scalaz.syntax.std.ToListOps

    with scalaz.syntax.ToValidationOps
    with scalaz.syntax.ToFunctorOps
    with scalaz.syntax.ToIdOps {

  val White = Color.White
  val Black = Color.Black

  type Direction = Pos => Option[Pos]
  type Directions = List[Direction]

  object Directions {
    // Named directions --
    val up:        Direction = _.up
    val left:      Direction = _.left
    val right:     Direction = _.right
    val down:      Direction = _.down
    val upLeft:    Direction = _.upLeft
    val downLeft:  Direction = _.downLeft
    val upRight:   Direction = _.upRight
    val downRight: Direction = _.downRight

    // Canonical directions --
    val cardinals = List(up, left, down, right)
    val diagonals = List(upLeft, upRight, downLeft, downRight)

    // Composition helpers --
    import scala.language.implicitConversions
    implicit def autoLiftDirection(dir: Pos => Option[Pos]): Option[Pos] => Option[Pos] = _ flatMap dir
  }


  type PieceMap = Map[Pos, Piece]

  type PositionHash = Array[Byte]

  object implicitFailures {
    implicit def stringToFailures(str: String): Failures = scalaz.NonEmptyList(str)
  }

  def parseIntOption(str: String): Option[Int] = try {
    Some(java.lang.Integer.parseInt(str))
  }
  catch {
    case e: NumberFormatException => None
  }
}
