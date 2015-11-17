class SodukoBoard(private var board: Map[Square, SodukoEntry] = Map()) {

  val seed = 3
  val boardSize = seed ^ 2

  private val set = (1 to boardSize).toSet

  def getEntry(square: Square) = board.getOrElse(square, SodukoOptions(seed))

  def setEntries(entries: Entry*) = entries.foreach(setEntry)

  def setEntry(entry: Entry) = {
    import entry._
    board = board.updated(square, value)

    boardRemoveOptionFromRow(square.row, value)
    boardRemoveOptionFromCol(square.col, value)
    boardRemoveOptionFromSquare(entry)
  }

  def printBoard: String = {
    val rowSeparator = "\n" + s"|${"_" * 19}" * 9 + "|\n"

    (0 until boardSize).map { row =>
      (0 until boardSize).map { col =>
        board.getOrElse(Square(row, col), SodukoOptions(seed)).printEntry
      }.mkString("|", "|", "|")
    }.mkString(s"${"_" * 181}\n", rowSeparator, rowSeparator)
  }

  def isSolved: Boolean =
    allRowsAreValid && allColsAreValid && allSquaresAreValid

  private def allRowsAreValid: Boolean =
    (0 until boardSize).forall(rowIsValid)

  private def rowIsValid(row: Int): Boolean =
    isValidSet(for (i <- 0 until boardSize) yield getEntry(Square(row, i)).getValue)

  private def allColsAreValid: Boolean =
    (0 until boardSize).forall(colIsValid)

  private def colIsValid(col: Int): Boolean =
    isValidSet(for (i <- 0 until boardSize) yield getEntry(Square(i, col)).getValue)


  private def allSquaresAreValid: Boolean = {
    {
      for (
        i <- 0 until boardSize by seed;
        j <- 0 until boardSize by seed
      ) yield Square(i, j)
    }.forall(squareIsValid)
  }

  private def squareIsValid(square: Square): Boolean = {
    isValidSet(
      for (
        i <- 0 until seed;
        j <- 0 until seed
      ) yield getEntry(square).getValue)
  }

  private def isValidSet(values: Seq[Option[Int]]) = values.filter(_.nonEmpty).map(_.get).toSet == set

  private def boardRemoveOptionFrom(entry: Entry) =
    getEntry(entry.square) match {
      case SodukoOptions(is) =>
        board = board.updated(entry.square, SodukoOptions(is.diff(Seq(entry.value))))
      case _ =>
    }

  private def boardRemoveOptionFromRow(row: Int, value: SodukoVal) = (0 until boardSize).foreach { i =>
    boardRemoveOptionFrom(Entry(Square(row, i), value))
  }

  private def boardRemoveOptionFromCol(col: Int, value: SodukoVal) = (0 until boardSize).foreach { i =>
    boardRemoveOptionFrom(Entry(Square(i, col), value))
  }

  private def boardRemoveOptionFromSquare(entry: Entry) = {
    import entry.square._
    val i_start = (row / seed) * seed
    val j_start = (col / seed) * seed
    for (
      i <- i_start until i_start + seed;
      j <- j_start until j_start + seed
    ) yield boardRemoveOptionFrom(Entry(Square(i, j), entry.value))
  }


}

