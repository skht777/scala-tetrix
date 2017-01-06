import scalafx.Includes._
import scalafx.animation.Timeline
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.{Canvas, GraphicsContext}
import scalafx.scene.paint.Color
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
  val max = (10, 20)
  val blockSize = 30
  val field = new Field(max)
  val size = (max._1 * blockSize, max._2 * blockSize)
  val gc: GraphicsContext = canvas getGraphicsContext2D
  val tl = new Timeline

  canvas.width = 400
  canvas.height = 600
  gc.fill = Color.Black
  gc.fillRect(0, 0, canvas getWidth, canvas getHeight)
  gc.stroke = Color.White
  1 to max._1 foreach(i => gc.strokeLine(blockSize * i, 0, blockSize * i, size._2))
  1 to max._2 foreach(i => gc.strokeLine(0, blockSize * i, size._1, blockSize * i))
  gc.fill = Color.White
  for (block <- field.view.blocks) gc.fillRect(block.pos._1 * blockSize, size._2 - block.pos._2 * blockSize, blockSize, blockSize)
}
