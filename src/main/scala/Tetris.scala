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

  private[this] def drawView() = {
    val blockSize = 30
    val nextSize = blockSize / 2
    val size: Point[Int] = max * blockSize
    val nextPos = Point(max.x + 1, 2) * blockSize
    gc.fill = Color.Black
    gc.fillRect(0, 0, canvas getWidth, canvas getHeight)
    gc.stroke = Color.White
    0 to max.x foreach (i => gc.strokeLine(blockSize * i, 0, blockSize * i, size.y))
    0 to max.y foreach (i => gc.strokeLine(0, blockSize * i, size.x, blockSize * i))
    // draw next block line
    0 to 4 foreach (i => {
      gc.strokeLine(nextPos.x + nextSize * i, nextPos.y, nextPos.x + nextSize * i, nextPos.y + nextSize * 4)
      gc.strokeLine(nextPos.x, nextPos.y + nextSize * i, nextPos.x + nextSize * 4, nextPos.y + nextSize * i)
    })
    gc.fill = Color.White
    gc.stroke = Color.Black
    state.view.blocks foreach (b => {
      val (x, y) = (b.pos.x * blockSize, (max.y - b.pos.y - 1) * blockSize)
      gc.fillRect(x, y, blockSize, blockSize)
      gc.strokeLine(x, y, x + blockSize, y)
      gc.strokeLine(x, y, x, y + blockSize)
      gc.strokeLine(x, y + blockSize, x + blockSize, y + blockSize)
      gc.strokeLine(x + blockSize, y, x + blockSize, y + blockSize)
    })
    // draw next block
    state.view.next.copy(pos = Point(2.0, 2.0)).current foreach (b => {
      val (x, y) = (nextPos.x + b.pos.x * nextSize, nextPos.y + (4 - b.pos.y - 1) * nextSize)
      gc.fillRect(x, y, nextSize, nextSize)
      gc.strokeLine(x, y, x + nextSize, y)
      gc.strokeLine(x, y, x, y + nextSize)
      gc.strokeLine(x, y + nextSize, x + nextSize, y + nextSize)
      gc.strokeLine(x + nextSize, y, x + nextSize, y + nextSize)
    })
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
      case _ => identity
    }
    transAndDraw(trans)
  }
}
