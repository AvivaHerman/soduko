import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope

class SodukoGameTest extends SpecificationWithJUnit {

  "SodukoGame" should {
    "create soduko board game" in new Context {
      var board: Map[Square, SodukoEntry] = Map()

      for (
        i <- 0 until 9;
        j <- 0 until 9
      ) yield (i, j) match {
        case (0,0) => board = board.updated(Square(0, 0), sodukoVal)
        case (k, 0) => board = board.updated(Square(k, 0), SodukoOptions((1 to 8).map(SodukoVal)))
        case (0, k) => board = board.updated(Square(0, k), SodukoOptions((1 to 8).map(SodukoVal)))
        case (n, m) if n < 3 && m < 3 => board = board.updated(Square(n,m), SodukoOptions((1 to 8).map(SodukoVal)))
        case (n, m) => board = board.updated(Square(n,m) ,SodukoOptions((1 to 9).map(SodukoVal)))
      }

//      println(game.printBoard)

      game.printBoard must beEqualTo(new SodukoBoard(board).printBoard)
    }
  }

  "soduko solution" should {
    "return false" in new Context {
      game.isSolved must beFalse
    }

    "return true" in new Context {
      solvedGame.isSolved must beTrue
    }
  }

  abstract class Context extends Scope {
    val sodukoVal = SodukoVal(9)
    val game = new SodukoGame(Entry(Square(0,0),sodukoVal))

    val values = (1 to 9).map(SodukoVal)

    val entries = (0 until 3).flatMap(i =>
      (0 until 3).flatMap( k =>
      (0 until 9).map(j => Entry(Square(i * 3 + k, j), values((i + k * 3 + j) % 9)))))

    val solvedGame = new SodukoGame(entries: _*)
  }
}
