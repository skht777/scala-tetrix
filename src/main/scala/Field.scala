import Kind.T

/**
  *
  * @author skht777
  *
  */
class Field(size: (Int, Int)) {
  def newState(blocks: Seq[Block] = Seq()): GameState = {
    def dropOffPos = (size._1 / 2.0, size._2 toDouble)

    val point = Point(T, dropOffPos)
    GameState(blocks ++ point.current, size, point)
  }

  val moveLeft = transit { _.moveBy(-1.0, 0.0) }
  val moveRight = transit { _.moveBy(1.0, 0.0) }
  val moveDown = transit { _.moveBy(0.0, -1.0) }
  val rotateCW = transit { _.rotateBy(-math.Pi / 2.0) }

  private[this] def transit(trans: Point => Point): GameState => GameState =
    (s: GameState) => validate(s.copy(
      blocks = unload(s.currentPiece, s.blocks),
      currentPiece = trans(s.currentPiece))) map (x =>
      x.copy(blocks = load(x.currentPiece, x.blocks))) getOrElse s

  private[this] def validate(s: GameState): Option[GameState] = {
    val size = s.gridSize

    def inBounds(pos: (Int, Int)): Boolean =
      (pos._1 >= 0) && (pos._1 < size._1) && (pos._2 >= 0) && (pos._2 <= size._2)

    val currentPoss = s.currentPiece.current map { _.pos }
    if ((currentPoss forall inBounds) &&
      (s.blocks map { _.pos } intersect currentPoss).isEmpty) Some(s)
    else None
  }

  private[this] def unload(p: Point, bs: Seq[Block]): Seq[Block] = {
    val currentPoss = p.current map { _.pos }
    bs filterNot { currentPoss contains _.pos }
  }

  private[this] def load(p: Point, bs: Seq[Block]): Seq[Block] = bs ++ p.current
}

case class GameView(blocks: Seq[Block], gridSize: (Int, Int), current: Point)

case class GameState(blocks: Seq[Block], gridSize: (Int, Int), currentPiece: Point) {
  def view: GameView = GameView(blocks, gridSize, currentPiece)
}