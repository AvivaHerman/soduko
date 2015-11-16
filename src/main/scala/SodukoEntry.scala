trait SodukoEntry {
  def printEntry: String
}

case class SodukoVal(i: Int) extends SodukoEntry {
  override def printEntry: String = s"${" "*9}$i${" "*9}"
}

case class SodukoOptions(is: Seq[SodukoVal] = (1 to 9).toSeq.map(SodukoVal)) extends SodukoEntry {
  override def printEntry: String = {
    val options = is.map(_.i).mkString(",")
    s"O:$options${" "*(17-options.length)}"
  }
}
