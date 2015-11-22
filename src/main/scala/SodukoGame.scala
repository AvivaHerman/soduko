
class SodukoGame {

  def print(board: SodukoBoard): String = board.toString

  def isSolved(board: SodukoBoard): Boolean = board.isSolved

  def solveGame(board: SodukoBoard): SodukoBoard = {
    board.getNextEntryWithSudokuOptions match {
      case Some(entryOptions) =>
        entryOptions.options.is.toList match {
          case i :: _ =>
            board.setEntry(Entry(entryOptions.square, i))
            solveGame(board)
        }
      case None => board
    }
  }

}
