import Kind._

import scala.annotation.tailrec

/**
  *
  * @author skht777
  *
  */
case class Field(size: Point[Int]) {
  val moveLeft = transit(_.moveBy(-1.0, 0.0))
  val moveRight = transit(_.moveBy(1.0, 0.0))
  val rotateCW = transit(_.rotateBy(-math.Pi / 2.0))
  val moveDown = transit(_.moveBy(0.0, -1.0),
    Function.chain(clearFullRow :: { s: State => newState(s.blocks) } :: Nil))

  def newState(blocks: Seq[Square] = Seq()) = {
    val dropPos = Point(size.x / 2.0, size.y)
    State(Block(random(scala.util.Random), dropPos), blocks).load()
  }

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

  private[this] lazy val clearFullRow = (s: State) => {
    def isFullRow(y: Int) = (s.blocks count { _.pos.y == y }) == size.x

    @tailrec def tryRow(y: Int, _s: State): State =
      if (y < 0) _s
      else if (isFullRow(y)) {
        val (below, above) = (_s.blocks filter { _.pos.y < y }, _s.blocks filter { _.pos.y > y })
        val down = (sq: Square) => sq.copy(pos = Point(sq.pos.x, sq.pos.y - 1))
        tryRow(y - 1, _s.copy(blocks = below ++ (above map down)))
      } else tryRow(y - 1, _s)

    tryRow(size.y - 1, s)
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