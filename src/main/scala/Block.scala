import Kind._

import scala.util.Random

/**
  *
  * @author skht777
  *
  */
case class Point[T](x: T, y: T)(implicit num: Numeric[T]) {

  import num._

  def map[U](func: T => U)(implicit u: Numeric[U]) = Point(func(x), func(y))

  def +[U <: T](other: Point[U]): Point[T] = Point(this.x + other.x, this.y + other.y)

  def +[U >: T](other: Point[U])(implicit u: Numeric[U]): Point[U] = Point[U](x, y) + other

  def *[U <: T](scalar: U): Point[T] = Point(x * scalar, y * scalar)

  def *[U >: T](scalar: U)(implicit u: Numeric[U]): Point[U] = Point[U](x, y) * scalar
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

  case object NBO extends Kind

  private[this] val values = Seq(I, J, L, S, Z, T, O)

  private[this] def randomStream: Stream[Kind] = random(Random) #:: randomStream

  private[this] def random(r: Random): Kind = values(r nextInt values.length)

  def apply() = randomStream.head
}

case class Square(kind: Kind, pos: Point[Int])

case class Block(kind: Kind, pos: Point[Double], locales: Seq[Point[Double]]) {
  def current: Seq[Square] = locales map (p => (p + pos) map math.floor map (_.toInt)) map (Square(kind, _))

  def moveBy(dx: Double, dy: Double) = copy(pos = pos + Point(dx, dy))

  def rotateBy(theta: Double) = {
    def rotate(v: Point[Double]) = {
      val (sin, cos) = (math sin theta, math cos theta)
      Point(v.x * cos - v.y * sin, v.x * sin + v.y * cos)
    }

    def roundToHalf(v: Point[Double]): Point[Double] = (v * 2.0 map math.round) map (_ * 0.5)

    copy(locales = locales map rotate map roundToHalf)
  }
}

case object Block {
  def apply(kind: Kind = Kind(), pos: Point[Double]): Block = {
    def p(x: Double, y: Double) = Point(x, y)

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

  def random(pos: Point[Double] = Point(0, 0)) = apply(pos = pos)
}