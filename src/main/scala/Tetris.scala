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
  val max = Point(10, 20)
  val blockSize = 30
  val field = Field(max)
  val size = max * blockSize
  val gc: GraphicsContext = jfxGraphicsContext2sfx(canvas getGraphicsContext2D)
  val tl = Timeline(Seq(KeyFrame(Duration(1000),
    onFinished = _ => transAndDraw(field.moveDown))))
  var state = field.newState()

  tl.cycleCount = Timeline.Indefinite
  drawView()

  def drawView() = {
    gc.fill = Color.Black
    gc.fillRect(0, 0, canvas getWidth, canvas getHeight)
    gc.stroke = Color.White
    1 to max.x foreach (i => gc.strokeLine(blockSize * i, 0, blockSize * i, size.y))
    1 to max.y foreach (i => gc.strokeLine(0, blockSize * i, size.x, blockSize * i))
    gc.fill = Color.White
    for (block <- state.view.blocks) gc.fillRect(block.pos.x * blockSize, (max.y - block.pos.y - 1) * blockSize, blockSize, blockSize)
  }

  def transAndDraw(trans: State => State) = {
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
