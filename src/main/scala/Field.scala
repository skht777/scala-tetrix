import Kind.T

/**
  *
  * @author skht777
  *
  */
class Field(size: Point[Int]) {
  def newState(blocks: Seq[Square] = Seq()): GameState = {
    val point = Block(T, Point(size.x / 2.0, size.y))
    GameState(blocks ++ point.current, point)
  }

  val moveLeft = transit { _.moveBy(-1.0, 0.0) }
  val moveRight = transit { _.moveBy(1.0, 0.0) }
  val moveDown = transit { _.moveBy(0.0, -1.0) }
  val rotateCW = transit { _.rotateBy(-math.Pi / 2.0) }

  private[this] def transit(trans: Block => Block,) = (s: GameState) =>
    validate(GameState(unload(s.current, s.blocks), trans(s.current))) map (x =>
      x.copy(blocks = load(x.current, x.blocks))) getOrElse s

  private[this] def validate(s: GameState): Option[GameState] = {
    def inBounds(pos: Point[Int]): Boolean =
      pos.x >= 0 && pos.x < size.x && pos.y >= 0 && pos.y <= size.y + 2

    val currentPoss = s.current.current map { _.pos }
    if ((currentPoss forall inBounds) &&
      (s.blocks map { _.pos } intersect currentPoss).isEmpty) Some(s)
    else None
  }

  private[this] def unload(p: Block, bs: Seq[Square]) = {
    val currentPoss = p.current map { _.pos }
    bs filterNot { currentPoss contains _.pos }
  }

  private[this] def load(p: Block, bs: Seq[Square]) = bs ++ p.current
}

case class GameView(blocks: Seq[Square], current: Block)

case class GameState(blocks: Seq[Square], current: Block) {
  def view: GameView = GameView(blocks, current)
}