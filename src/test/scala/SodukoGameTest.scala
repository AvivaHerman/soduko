import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope

class SodukoGameTest extends SpecificationWithJUnit {

  "SodukoGame" should {
    "solve a game board" in new Context {
      val board = new SodukoBoard()
      board.setEntries(
        Entry(Square(0,0), SodukoVal(6)),
        Entry(Square(0,2), SodukoVal(8)),
        Entry(Square(0,3), SodukoVal(5)),
        Entry(Square(0,8), SodukoVal(3)),
        Entry(Square(1,1), SodukoVal(2)),
        Entry(Square(1,4), SodukoVal(3)),
        Entry(Square(1,7), SodukoVal(8)),
        Entry(Square(2,2), SodukoVal(3)),
        Entry(Square(2,3), SodukoVal(4)),
        Entry(Square(2,6), SodukoVal(1)),
        Entry(Square(2,8), SodukoVal(2)),
        Entry(Square(3,0), SodukoVal(1)),
        Entry(Square(3,5), SodukoVal(8)),
        Entry(Square(3,8), SodukoVal(9)),
        Entry(Square(4,1), SodukoVal(4)),
        Entry(Square(4,7), SodukoVal(3)),
        Entry(Square(5,0), SodukoVal(8)),
        Entry(Square(5,2), SodukoVal(5)),
        Entry(Square(5,3), SodukoVal(7)),
        Entry(Square(5,5), SodukoVal(3)),
        Entry(Square(5,8), SodukoVal(4)),
        Entry(Square(6,0), SodukoVal(2)),
        Entry(Square(6,2), SodukoVal(4)),
        Entry(Square(6,5), SodukoVal(1)),
        Entry(Square(6,6), SodukoVal(9)),
        Entry(Square(7,1), SodukoVal(9)),
        Entry(Square(7,4), SodukoVal(5)),
        Entry(Square(7,7), SodukoVal(2)),
        Entry(Square(8,0), SodukoVal(3)),
        Entry(Square(8,3), SodukoVal(9)),
        Entry(Square(8,6), SodukoVal(6)),
        Entry(Square(8,8), SodukoVal(5))
      )
      
      game.isSolved(board) must beFalse

      game.solveGame(board)

      game.isSolved(board) must beTrue
    }
  }

  "soduko solution" should {
    "return false" in new Context {
      val board = new SodukoBoard()
      board.setEntry(Entry(Square(0,0),sodukoVal))
      
      game.isSolved(board) must beFalse
    }

    "return true" in new Context {
      val solvedBoard = new SodukoBoard()
      solvedBoard.setEntries(entries: _*)
      
      game.isSolved(solvedBoard) must beTrue
    }
  }

  abstract class Context extends Scope {
    val sodukoVal = SodukoVal(9)

    val values = (1 to 9).map(SodukoVal)

    val entries = (0 until 3).flatMap(i =>
      (0 until 3).flatMap( k =>
      (0 until 9).map(j => Entry(Square(i * 3 + k, j), values((i + k * 3 + j) % 9)))))

    val game = new SodukoGame()
  }
}
