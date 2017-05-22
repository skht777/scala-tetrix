import scalafx.Includes._
import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.paint.Color
import scalafx.util.Duration
import scalafxml.core.macros.sfxml
import scalafxml.core.{FXMLView, NoDependencyResolver}

/**
  *
  * @author skht777
  *
  */
object Tetris extends JFXApp {

  stage = new PrimaryStage {
    title = "tetris"
    scene = new Scene(FXMLView(getClass.getResource("root.fxml"), NoDependencyResolver))
  }
  stage.getScene.getRoot.requestFocus()
}

@sfxml
class fieldController(private val canvas: Canvas) {
  private[this] val unit = ViewUnit(Point(10, 20))
  private[this] val gc: GraphicsContext = jfxGraphicsContext2sfx(canvas getGraphicsContext2D)
  private[this] val tl = Timeline(Seq(KeyFrame(Duration(1000), onFinished = _ => transAndDraw(unit.down()))))

  tl.cycleCount = Timeline.Indefinite
  tl.play()
  drawView()

  private[this] def drawBlock(posX: Double, posY: Double, size: Int, exX: Double = 0, exY: Double = 0) = {
    val p = Point(exX, exY) + Point(posX, posY) * size
    gc.fill = Color.White
    gc.stroke = Color.Black
    gc.fillRect(p.x, p.y, size, size)
    strokeLine(p.x, p.y, size, 0)
    strokeLine(p.x, p.y, 0, size)
    strokeLine(p.x, p.y + size, size, 0)
    strokeLine(p.x + size, p.y, 0, size)
  }

  private[this] def strokeLine(x: Double, y: Double, w: Double, h: Double) = gc.strokeLine(x, y, x + w, y + h)

  private[this] def strokeLine(w: Int, h: Int, size: Int, exX: Double = 0, exY: Double = 0) = {
    gc.fill = Color.Black
    gc.stroke = Color.White
    0 to w map (i => i * size) foreach (x => strokeLine(exX + x, exY, 0, size * h))
    0 to h map (i => i * size) foreach (y => strokeLine(exX, exY + y, size * w, 0))
  }

  private[this] def drawView() = {
    val blockSize = 30
    val nextSize = blockSize / 2
    val size: Point[Int] = unit.size * blockSize
    val nextPos = Point(unit.size.x + 1, 2) * blockSize
    gc.fill = Color.Black
    gc.fillRect(0, 0, canvas getWidth, canvas getHeight)
    strokeLine(unit.size.x, unit.size.y, blockSize)
    unit.view.blocks foreach (b => drawBlock(b.pos.x, unit.size.y - b.pos.y - 1, blockSize))

    // draw next block
    strokeLine(4, 4, nextSize, nextPos.x, nextPos.y)
    unit.view.next.copy(pos = Point(2.0, 2.0))
      .current foreach (b => drawBlock(b.pos.x, 4 - b.pos.y - 1, nextSize, nextPos.x, nextPos.y))

    gc.stroke = Color.White
    unit.view.status match {
      case Status.Ready => gc.strokeText("Press the Enter", size.x + 10, size.y / 2)
      case Status.GameOver => gc.strokeText("Game Over", size.x + 10, size.y / 2)
      case _ =>
    }
  }

  private[this] def transAndDraw(trans: State => State) = {
    unit.trans(trans)
    drawView()
  }

  def operate(key: KeyEvent): Unit = {
    val trans: State => State = jfxKeyCode2sfx(key.getCode) match {
      case KeyCode.Left => unit.left()
      case KeyCode.Right => unit.right()
      case KeyCode.Up => unit.rotate()
      case KeyCode.Down => unit.down()
      case KeyCode.Space => identity
      case KeyCode.Enter => unit.status()
      case _ => identity
    }
    transAndDraw(trans)
  }
}
