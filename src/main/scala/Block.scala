import Kind._

import scala.util.Random
import scalafx.scene.paint.Color

/**
  *
  * @author skht777
  *
  */
case class Block(kind: Kind, pos: Coordinate[Int])

case class Coordinate[T](x: T, y: T)(implicit num: Numeric[T]) {

  import num._

  def map[U: Numeric](func: T => U) = Coordinate(func(x), func(y))

  def +(other: Coordinate[T]) = Coordinate(this.x + other.x, this.y + other.y)

  def *[U: Numeric](scalar: U): Coordinate[T] = Coordinate(x * scalar.asInstanceOf[T], y * scalar.asInstanceOf[T])
}

case class Point(kind: Kind, pos: Coordinate[Double], locales: Seq[Coordinate[Double]]) {
  def current: Seq[Block] = locales map { p => Block(kind, (p + pos).map(c => math.floor(c) toInt)) }

  def moveBy(dx: Double, dy: Double) = copy(pos = pos + Coordinate(dx, dy))

  def rotateBy(theta: Double) = {
    def rotate(v: Coordinate[Double]) = {
      val (sin, cos) = (math sin theta, math cos theta)
      Coordinate(v.x * cos - v.y * sin, v.x * sin + v.y * cos)
    }

    def roundToHalf(v: Coordinate[Double]) = v.map(c => math.round(c * 2) * 0.5)

    copy(locales = locales map rotate map roundToHalf)
  }
}

case object Point {
  def apply(kind: Kind, pos: Coordinate[Double]): Point = {
    def p(x: Double, y: Double) = Coordinate(x, y)

    Point(kind, pos, kind match {
      case I => Seq(p(-1.5, 0.0), p(-0.5, 0.0), p(0.5, 0.0), p(1.5, 0.0))
      case J => Seq(p(-1.0, -0.5), p(0.0, -0.5), p(1.0, -0.5), p(-1.0, 0.5))
      case L => Seq(p(-1.0, -0.5), p(0.0, -0.5), p(1.0, -0.5), p(1.0, 0.5))
      case S => Seq(p(0.0, 0.5), p(-1.0, -0.5), p(1.0, 0.5), p(0.0, -0.5))
      case Z => Seq(p(0.0, 0.5), p(-1.0, 0.5), p(1.0, -0.5), p(0.0, -0.5))
      case T => Seq(p(-1.0, 0.0), p(0.0, 0.0), p(1.0, 0.0), p(0.0, 1.0))
      case O => Seq(p(-0.5, 0.5), p(0.5, 0.5), p(-0.5, -0.5), p(0.5, -0.5))
    })
  }
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

  private val values = Seq(I, J, L, S, Z, T, O)

  def apply(x: Int): Kind = values(x)

  def random(r: Random): Kind = Kind(r nextInt values.length)

  def ordinal(x: Kind): Int = values indexOf x

}

object BlockColor {
  def apply(kind: Kind): Color = kind match {
    case I => Color.White
    case J => Color.White
    case L => Color.White
    case S => Color.White
    case Z => Color.White
    case T => Color.White
    case O => Color.White
    case NBO => Color.White
  }
}