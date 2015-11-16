class SodukoGame(entries: Entry*) {

  val board = new SodukoBoard()
  board.setEntries(entries: _*)

  def printBoard: String = board.printBoard

}
