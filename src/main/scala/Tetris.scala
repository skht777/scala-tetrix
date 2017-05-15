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
  private[this] val max = Point(10, 20)
  private[this] val field = Field(max)
  private[this] val gc: GraphicsContext = jfxGraphicsContext2sfx(canvas getGraphicsContext2D)
  private[this] val tl = Timeline(Seq(KeyFrame(Duration(1000),
    onFinished = _ => transAndDraw(field.moveDown))))
  private[this] var state: State = field.initState

  tl.cycleCount = Timeline.Indefinite
  drawView()

  private[this] def drawBlock(posX: Int, posY: Int, size: Int, exX: Int = 0, exY: Int = 0) = {
    val (x, y) = (exX + posX * size, exY + posY * size)
    gc.fill = Color.White
    gc.stroke = Color.Black
    gc.fillRect(x, y, size, size)
    gc.strokeLine(x, y, x + size, y)
    gc.strokeLine(x, y, x, y + size)
    gc.strokeLine(x, y + size, x + size, y + size)
    gc.strokeLine(x + size, y, x + size, y + size)
  }

  private[this] def drawLine(w: Int, h: Int, size: Int, exX: Int = 0, exY: Int = 0) = {
    gc.fill = Color.Black
    gc.stroke = Color.White
    0 to w foreach (i => gc.strokeLine(exX + size * i, exY, exX + size * i, exY + size * h))
    0 to h foreach (i => gc.strokeLine(exX, exY + size * i, exX + size * w, exY + size * i))
  }

  private[this] def drawView() = {
    val blockSize = 30
    val nextSize = blockSize / 2
    val size: Point[Int] = max * blockSize
    val nextPos = Point(max.x + 1, 2) * blockSize
    gc.fill = Color.Black
    gc.fillRect(0, 0, canvas getWidth, canvas getHeight)
    drawLine(max.x, max.y, blockSize)
    state.view.blocks foreach (b => drawBlock(b.pos.x, max.y - b.pos.y - 1, blockSize))

    // draw next block
    drawLine(4, 4, nextSize, nextPos.x, nextPos.y)
    state.view.next.copy(pos = Point(2.0, 2.0))
      .current foreach (b => drawBlock(b.pos.x, 4 - b.pos.y - 1, nextSize, nextPos.x, nextPos.y))

    gc.stroke = Color.White
    state.status match {
      case Status.Ready => gc.strokeText("Press the Enter", size.x + 10, size.y / 2)
      case Status.GameOver => gc.strokeText("Game Over", size.x + 10, size.y / 2)
      case _ =>
    }
  }

  private[this] def transAndDraw(trans: State => State) = {
    state = trans(state)
    drawView()
  }

  def operate(key: KeyEvent): Unit = {
    if (key.getCode == KeyCode.Enter.delegate) tl.play()
    val trans: State => State = jfxKeyCode2sfx(key.getCode) match {
      case KeyCode.Left => field.moveLeft
      case KeyCode.Right => field.moveRight
      case KeyCode.Up => field.rotateCW
      case KeyCode.Down => field.moveDown
      case KeyCode.Space => identity
      case KeyCode.Enter => field.transStatus
      case _ => identity
    }
    transAndDraw(trans)
  }
}
