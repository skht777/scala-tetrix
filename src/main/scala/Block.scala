import Kind._

import scala.util.Random

/**
  *
  * @author skht777
  *
  */
case class Point[T](x: T, y: T)(implicit num: Numeric[T]) {

  import num._

  def map[U](func: T => U)(implicit u: Numeric[U]): Point[U] = copy(func(x), func(y))

  def +[U <: T](other: Point[U]): Point[T] = copy(x + other.x, y + other.y)

  def +[U >: T](other: Point[U])(implicit u: Numeric[U]): Point[U] = copy[U](x, y) + other

  def move[U <: T](x: U, y: U): Point[T] = this + Point(x, y)

  def move[U >: T](x: U, y: U)(implicit u: Numeric[U]): Point[U] = this + Point(x, y)

  def *[U](scalar: U)(implicit ev$1: U => T): Point[T] = map(_ * scalar)
}

sealed trait Kind

object Kind {

  case object I extends Kind

  case object J extends Kind

  case object L extends Kind

  case object S extends Kind

  case object Z extends Kind

  case object T extends Kind

  case object O extends Kind

//  case object NBO extends Kind

  private[this] val values = Seq(I, J, L, S, Z, T, O)

  private[this] def randomStream: Stream[Kind] = random(Random) #:: randomStream

  private[this] def random(r: Random): Kind = values(r nextInt values.length)

  def apply() = Some(randomStream.head).getOrElse(I)
}

case class Square(kind: Kind, pos: Point[Int])

case class Block(kind: Kind, pos: Point[Double], locales: Seq[Point[Double]]) {
  def current: Seq[Square] = locales map (p => (p + pos) map math.floor map (_.toInt)) map (Square(kind, _))

  def moveBy(dx: Double, dy: Double): Block = copy(pos = pos.move(dx, dy))

  def rotateBy(theta: Double): Block = {
    def rotate(v: Point[Double]) = {
      val (vs, vc) = (v * (math sin theta), v * (math cos theta))
      Point(vc.x - vs.y, vs.x + vc.y)
    }

    def roundToHalf(v: Point[Double]): Point[Double] = (v * 2.0 map math.round) map (_ * 0.5)

    copy(locales = locales map rotate map roundToHalf)
  }
}

case object Block {
  def apply(kind: Kind = Kind(), pos: Point[Double]): Block = {
    val p: (Double, Double) => Point[Double] = Point.apply _

    Block(kind, pos, kind match {
      case I => Seq(p(-1.5, 0.0), p(-0.5, 0.0), p(0.5, 0.0), p(1.5, 0.0))
      case J => Seq(p(-1.0, -0.5), p(0.0, -0.5), p(1.0, -0.5), p(-1.0, 0.5))
      case L => Seq(p(-1.0, -0.5), p(0.0, -0.5), p(1.0, -0.5), p(1.0, 0.5))
      case S => Seq(p(0.0, 0.5), p(-1.0, -0.5), p(1.0, 0.5), p(0.0, -0.5))
      case Z => Seq(p(0.0, 0.5), p(-1.0, 0.5), p(1.0, -0.5), p(0.0, -0.5))
      case T => Seq(p(-1.0, -0.5), p(0.0, -0.5), p(1.0, -0.5), p(0.0, 0.5))
      case O => Seq(p(-0.5, 0.5), p(0.5, 0.5), p(-0.5, -0.5), p(0.5, -0.5))
    })
  }

  def random(pos: Point[Double] = Point(0.0, 0.0)): Block = apply(pos = pos)
}