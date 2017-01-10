import scalafx.Includes.{jfxKeyCode2sfx, _}
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
}

@sfxml
class fieldController(private val canvas: Canvas) {
  val max = Coordinate(10, 20)
  val blockSize = 30
  val field = new Field(max)
  val size = max * blockSize
  val gc: GraphicsContext = jfxGraphicsContext2sfx(canvas getGraphicsContext2D)
  val tl = new Timeline
  var state = field.newState()

  tl.keyFrames = Seq(KeyFrame(Duration(1000), onFinished = _ => {
    state = field moveDown state
    drawView
  }))
  tl.cycleCount = Timeline.Indefinite
  canvas.width = 400
  canvas.height = 600
  canvas.requestFocus()
  drawView
  tl play

  def drawView = {
    gc.fill = Color.Black
    gc.fillRect(0, 0, canvas getWidth, canvas getHeight)
    gc.stroke = Color.White
    1 to max.x foreach (i => gc.strokeLine(blockSize * i, 0, blockSize * i, size.y))
    1 to max.y foreach (i => gc.strokeLine(0, blockSize * i, size.x, blockSize * i))
    gc.fill = Color.White
    for (block <- state.view.blocks) gc.fillRect(block.pos.x * blockSize, (max.y - block.pos.y - 1) * blockSize, blockSize, blockSize)
  }

  def operate(key: KeyEvent): Unit = {
    state = jfxKeyCode2sfx(key getCode) match {
      case KeyCode.Left => field moveLeft state
      case KeyCode.Right => field moveRight state
      case KeyCode.Up => field rotateCW state
      case KeyCode.Down => field moveDown state
      case KeyCode.Space => state
      case _ => state
    }
    drawView
  }
}
