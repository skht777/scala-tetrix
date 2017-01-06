import Kind.T

/**
  *
  * @author skht777
  *
  */
class Field(size: (Int, Int)) {
  private[this] def dropOffPos = (size._1 / 2.0, size._2.toDouble)
  private[this] var current = Point(T, dropOffPos).current
  private[this] var blocks = current
  def view: GameView = GameView(blocks, size, current)
}