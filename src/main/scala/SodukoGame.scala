class SodukoGame(entries: Entry*) {

  val board = new SodukoBoard()
  board.setEntries(entries: _*)

  override def toString: String = board.toString

  def isSolved: Boolean = board.isSolved

}
