package chess

import Pos._

class PosTest extends ChessTest {

  "A position" should {

    "be used to derive a relative list of positions" in {
      "D4 >| false" in { D4 moveRight (_ => false) must contain(E4, F4, G4, H4) }
      "D4 |< false" in { D4 moveLeft (_ => false) must contain(C4, B4, A4) }
      "D4 >| (==G4)" in { D4 moveRight (G4 ==) must contain(E4, F4, G4) }
      "D4 |< (==C4)" in { D4 moveLeft (C4 ==) must contain(C4) }
    }

    "be a string" in {
      "D5" in { D5.toString must_== "d5" }
    }
  }
}
