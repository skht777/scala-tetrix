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
    Function.chain(clearFullRow :: newState :: Nil))
  val transStatus = (s: State) => s.status match {
    case Status.Ready => s.copy(status = Status.Active)
    case Status.GameOver => initState
  }

  def initState: State = newState(State(next = Block.random(), status = Status.Ready))

  private[this] lazy val newState = (s: State) => {
    val dropPos = Point(size.x / 2.0, size.y + 1)
    val current = s.copy(s.next.copy(pos = dropPos), Block.random())

    validate(current) map (vs => vs.load()) getOrElse current.copy(status = Status.GameOver).load()
  }

  private[this] def transit(trans: Block => Block,
                            onFail: State => State = identity) =
    (s: State) => s.status match {
      case Status.Active => validate(s.unload()
        .copy(current = trans(s.current))) map (_.load()) getOrElse onFail(s)
      case _ => s
    }

  private[this] def validate(s: State): Option[State] = {
    def inBounds(pos: Point[Int]): Boolean =
      pos.x >= 0 && pos.x < size.x && pos.y >= 0 && pos.y <= size.y + 2

    if ((s.currentPos forall inBounds) &&
      (s.blocksPos intersect s.currentPos).isEmpty) Some(s)
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

case class View(current: Block, next: Block, blocks: Seq[Square])

case class State(current: Block = null, next: Block, blocks: Seq[Square] = Seq(), status: Status = Status.Active) {
  def view: View = View(current, next, blocks)

  def currentPos = current.current map { _.pos }

  def blocksPos = blocks map { _.pos }

  def load(b: Block = current) = copy(blocks = blocks ++ b.current)

  def unload(b: Block = current) = copy(blocks =
    blocks filterNot { b.current map { _.pos } contains _.pos })
}

sealed trait Status

object Status {

  case object Active extends Status

  case object Ready extends Status

  case object GameOver extends Status

}

