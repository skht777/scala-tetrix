import Kind._

import scala.util.Random
import scalafx.scene.paint.Color

/**
  *
  * @author skht777
  *
  */
case class Block(kind: Kind, pos: (Int, Int))

case class Point(kind: Kind, pos: (Double, Double), locales: Seq[(Double, Double)]) {
  def current: Seq[Block] = locales map { case (x, y) =>
    Block(kind, (math.floor(x + pos._1) toInt, math.floor(y + pos._2) toInt))
  }

  def moveBy(d: (Double, Double)): Point = copy(pos = (pos._1 + d._1, pos._2 + d._2))

  def rotateBy(theta: Double): Point = {
    val c = math cos theta
    val s = math sin theta

    def roundToHalf(v: (Double, Double)): (Double, Double) =
      (math.round(v._1 * 2.0) * 0.5, math.round(v._2 * 2.0) * 0.5)

    copy(locales = locales map { case (x, y) => (x * c - y * s, x * s + y * c) } map roundToHalf)
  }
}

case object Point {
  def apply(kind: Kind, pos: (Double, Double)):Point = Point(kind, pos, kind match {
    case I => Seq((-1.5, 0), (-0.5, 0), (0.5, 0), (1.5, 0))
    case J => Seq((-1.0, -0.5), (0.0, -0.5), (1.0, -0.5), (-1.0, 0.5))
    case L => Seq((-1.0, -0.5), (0.0, -0.5), (1.0, -0.5), (1.0, 0.5))
    case S => Seq((0.0, 0.5), (-1.0, -0.5), (1.0, 0.5), (0.0, -0.5))
    case Z => Seq((0.0, 0.5), (-1.0, 0.5), (1.0, -0.5), (0.0, -0.5))
    case T => Seq((-1.0, 0.0), (0.0, 0.0), (1.0, 0.0), (0.0, 1.0))
    case O => Seq((-0.5, 0.5), (0.5, 0.5), (-0.5, -0.5), (0.5, -0.5))
  })
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