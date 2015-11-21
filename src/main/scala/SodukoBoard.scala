class SodukoBoard(private var board: Map[Square, SodukoEntry] = Map()) {

  val seed = 3
  val boardSize = seed * seed

  private val set = (1 to boardSize).toSet

  def getEntry(square: Square) = board.getOrElse(square, SodukoOptions(seed))

  def setEntries(entries: Entry*) = entries.foreach(setEntry)

  def setEntry(entry: Entry): Unit = {
    import entry._
    board = board.updated(square, value)

    boardRemoveOptionFromRow(square.row, value)
    boardRemoveOptionFromCol(square.col, value)
    boardRemoveOptionFromSquare(entry)
  }

  override def toString: String = {
    val rowSeparator = "\n" + s"|${"_" * 19}" * 9 + "|\n"

    (0 until boardSize).map { row =>
      (0 until boardSize).map { col =>
        board.getOrElse(Square(row, col), SodukoOptions(seed)).printEntry
      }.mkString("|", "|", "|")
    }.mkString(s"${"_" * 181}\n", rowSeparator, rowSeparator)
  }

  def isSolved: Boolean =
    allRowsAreValid && allColsAreValid && allSquaresAreValid

  def allRowsAreValid: Boolean =
    (0 until boardSize).forall(rowIsValid)

  def rowIsValid(row: Int): Boolean =
    isValidSet(for (i <- 0 until boardSize) yield getEntry(Square(row, i)).getValue)

  def allColsAreValid: Boolean =
    (0 until boardSize).forall(colIsValid)

  def colIsValid(col: Int): Boolean =
    isValidSet(for (i <- 0 until boardSize) yield getEntry(Square(i, col)).getValue)


  def allSquaresAreValid: Boolean = {
    {
      for (
        i <- 0 until boardSize by seed;
        j <- 0 until boardSize by seed
      ) yield Square(i, j)
    }.forall(squareIsValid)
  }

  def squareIsValid(square: Square): Boolean = {
    isValidSet(
      for (
        i <- square.row until square.row + seed;
        j <- square.col until square.col + seed
      ) yield getEntry(Square(i,j)).getValue)
  }

  private def isValidSet(values: Seq[Option[Int]]) = values.filter(_.nonEmpty).map(_.get).toSet == set

  private def boardRemoveOptionFrom(entry: Entry): Unit =
    getEntry(entry.square) match {
      case SodukoOptions(options) =>
        options.diff(Seq(entry.value)).toList match {
          case lastOption :: Nil =>
            setEntry(entry.copy(value = lastOption))
          case otherOptions =>
            board = board.updated(entry.square, SodukoOptions(otherOptions))
        }
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

