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
  val field = Seq(10, 20)
  val block = 30
  val size = Seq(field.head * block, field.last * block)
  val gc: GraphicsContext = canvas.getGraphicsContext2D
  val tl: Timeline = new Timeline()

  canvas.width = size.head
  canvas.height = size.last
  gc.fill = Color.Black
  gc.fillRect(0, 0, size.head, size.last)
  gc.stroke = Color.White
  1 to field.head foreach (i => gc.strokeLine(block * i, 0, block * i, size.last))
  1 to field.last foreach(i => gc.strokeLine(0, block * i, size.head, block * i))
}
