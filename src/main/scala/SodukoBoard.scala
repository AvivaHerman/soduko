class SodukoBoard(private var board: Map[Tuple2[Int, Int], SodukoEntry] = Map()) {

  def getEntry(row: Int, col: Int) = board.getOrElse((row, col), SodukoOptions())

  def setEntries(entries: Entry*) = entries.foreach(setEntry)

  def setEntry(entry: Entry) = {
    import entry._
    board = board.updated((row, col), value)

    boardRemoveOptionFromRow(row, value)
    boardRemoveOptionFromCol(col, value)
    boardRemoveOptionFromSquare(entry)
  }

  def printBoard: String = {
    val rowSeparator = "\n" + s"|${"_" * 19}" * 9 + "|\n"

    (0 until 9).map { row =>
      (0 until 9).map { col =>
        board.getOrElse((row, col), SodukoOptions()).printEntry
      }.mkString("|", "|", "|")
    }.mkString(s"${"_" * 181}\n", rowSeparator, rowSeparator)
  }

  private def boardRemoveOptionFrom(entry: Entry) =
    board.getOrElse((entry.row, entry.col), SodukoOptions()) match {
      case x: SodukoOptions =>
        board = board.updated((entry.row, entry.col), SodukoOptions(x.is.diff(Seq(entry.value))))
      case _ =>
    }

  private def boardRemoveOptionFromRow(row: Int, value: SodukoVal) = (0 until 9).foreach { i =>
    boardRemoveOptionFrom(Entry(row, i, value))
  }

  private def boardRemoveOptionFromCol(col: Int, value: SodukoVal) = (0 until 9).foreach { i =>
    boardRemoveOptionFrom(Entry(i, col, value))
  }

  private def boardRemoveOptionFromSquare(entry: Entry) = {
    import entry._
    for (
      x <- Seq(1);
      i_start = (row / 3) * 3;
      j_start = (col / 3) * 3;
      i <- i_start until i_start + 3;
      j <- j_start until j_start + 3
    ) yield boardRemoveOptionFrom(Entry(i, j, value))
  }


}

