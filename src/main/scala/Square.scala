import Kind._

import scala.util.Random
import scalafx.scene.paint.Color

/**
  *
  * @author skht777
  *
  */
case class Square(kind: Kind, pos: Point[Int])

case class Point[T](x: T, y: T)(implicit num: Numeric[T]) {

  import num._

  def map[U](func: T => U)(implicit u: Numeric[U]) = Point(func(x), func(y))

  def +(other: Point[T]) = Point(this.x + other.x, this.y + other.y)

  def *[U <: T](scalar: U) = Point(x * scalar, y * scalar)

  def typeToInt = map { _.toInt }

  def typeToDouble = map { _.toDouble }
}

case class Block(kind: Kind, pos: Point[Double], locales: Seq[Point[Double]]) {
  def current: Seq[Square] = locales map { p => Square(kind, (p + pos) map math.floor typeToInt) }

  def moveBy(dx: Double, dy: Double) = copy(pos = pos + Point(dx, dy))

  def rotateBy(theta: Double) = {
    def rotate(v: Point[Double]) = {
      val (sin, cos) = (math sin theta, math cos theta)
      Point(v.x * cos - v.y * sin, v.x * sin + v.y * cos)
    }

    def roundToHalf(v: Point[Double]) = (v * 2 map math.round).typeToDouble * 0.5

    copy(locales = locales map rotate map roundToHalf)
  }
}

case object Block {
  def apply(kind: Kind, pos: Point[Double]): Block = {
    def p(x: Double, y: Double) = Point(x, y)

    Block(kind, pos, kind match {
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