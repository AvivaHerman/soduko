import org.specs2.specification.Scope
import org.specs2.mutable.Specification

class SodukoBoardTest extends Specification {

  "SodukoBoard" should {
    "validate row" in new Context {
      board.rowIsValid(0) must beFalse

      board.setEntries((1 to 9).map(i => Entry(Square(0, i - 1), SodukoVal(i))) : _*)

      board.rowIsValid(0) must beTrue
    }

    "validate col" in new Context {
      board.colIsValid(0) must beFalse

      board.setEntries((1 to 9).map(i => Entry(Square(i - 1, 0), SodukoVal(i))) : _*)

      board.colIsValid(0) must beTrue
    }

    "validate square" in new Context {
      board.squareIsValid(Square(0, 0)) must beFalse

      board.setEntries((0 until 3).flatMap(i => (0 until 3).map(j => Entry(Square(i, j), SodukoVal(i * 3 + j + 1)))) : _*)

      board.squareIsValid(Square(0, 0)) must beTrue
    }

    "set entry to soduku value if it has only one option value" in new Context {
      board.setEntries((1 to 8).map(i => Entry(Square(0, i - 1), SodukoVal(i))) : _*)

      board.getEntry(Square(0,8)) must beEqualTo(SodukoVal(9))
    }
  }

  abstract class Context extends Scope {
    val board = new SodukoBoard()
  }
}
