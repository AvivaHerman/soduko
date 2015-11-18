trait SodukoEntry {
  def printEntry: String
  def isValid: Boolean = false
  def getValue: Option[Int]
}

case class SodukoVal(i: Int) extends SodukoEntry {
  override def printEntry: String = s"${" "*9}$i${" "*9}"
  override def isValid: Boolean = true

  override def getValue: Option[Int] = Some(i)
}

case class SodukoOptions(is: Seq[SodukoVal]) extends SodukoEntry {
  override def printEntry: String = {
    val options = is.map(_.i).mkString(",")
    s"O:$options${" "*(17-options.length)}"
  }

  override def getValue: Option[Int] = None
}

object SodukoOptions {
  def apply(seed: Int): SodukoOptions = SodukoOptions((1 to (seed * seed)).toSeq.map(SodukoVal))
}
