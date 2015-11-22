case class Square(row: Int, col: Int)

case class Entry(square: Square, value: SodukoVal)
case class EntryOptions(square: Square, options: SodukoOptions)
