import Kind._

/**
  *
  * @author skht777
  *
  */
case class Field(size: Point[Int]) {
  def newState(blocks: Seq[Square] = Seq()): State = {
    val dropPos = Point(size.x / 2.0, size.y)
    State(Block(random(scala.util.Random), dropPos), blocks).load()
  }

  val moveLeft = transit(_.moveBy(-1.0, 0.0))
  val moveRight = transit(_.moveBy(1.0, 0.0))
  val rotateCW = transit(_.rotateBy(-math.Pi / 2.0))
  val moveDown = transit(_.moveBy(0.0, -1.0), s => newState(s.blocks))

  private[this] def transit(trans: Block => Block,
                            onFail: State => State = identity) =
    (s: State) => validate(s.unload()
      .copy(current = trans(s.current))) map (_.load()) getOrElse onFail(s)

  private[this] def validate(s: State): Option[State] = {
    def inBounds(pos: Point[Int]): Boolean =
      pos.x >= 0 && pos.x < size.x && pos.y >= 0 && pos.y <= size.y + 2

    if ((s.currentPos forall inBounds) &&
      (s.blocsPos intersect s.currentPos).isEmpty) Some(s)
    else None
  }
}

case class View(current: Block, blocks: Seq[Square])

case class State(current: Block, blocks: Seq[Square]) {
  def view: View = View(current, blocks)

  def currentPos = current.current map { _.pos }

  def blocsPos = blocks map { _.pos }

  def load(b: Block = current) = copy(blocks = blocks ++ b.current)

  def unload(b: Block = current) = copy(blocks =
    blocks filterNot { b.current map { _.pos } contains _.pos })
}