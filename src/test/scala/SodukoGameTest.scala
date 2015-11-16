import org.specs2.mutable.SpecificationWithJUnit

class SodukoGameTest extends SpecificationWithJUnit {

  "SodukoGame" should {
    "create empty soduko board game" in {
      val game = new SodukoGame

      println(game.printBoard)

      game.printBoard must beEqualTo(new SodukoBoard().printBoard)
    }

    "create soduko board game" in {
      val sodukoVal = SodukoVal(9)
      val game = new SodukoGame(Entry(0,0,sodukoVal))

      var board: Map[Tuple2[Int, Int], SodukoEntry] = Map()

      for (
        i <- 0 until 9;
        j <- 0 until 9
      ) yield (i, j) match {
        case (0,0) => board = board.updated((0, 0), sodukoVal)
        case (k, 0) => board = board.updated((k, 0), SodukoOptions((1 to 8).map(SodukoVal)))
        case (0, k) => board = board.updated((0, k), SodukoOptions((1 to 8).map(SodukoVal)))
        case (n, m) if n < 3 && m < 3 => board = board.updated((n,m), SodukoOptions((1 to 8).map(SodukoVal)))
        case x => board = board.updated(x ,SodukoOptions((1 to 9).map(SodukoVal)))
      }

      println(game.printBoard)

      game.printBoard must beEqualTo(new SodukoBoard(board).printBoard)
    }

    "validate board" in pending


  }

}
