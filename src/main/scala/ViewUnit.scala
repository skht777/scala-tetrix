/**
  *
  * @author skht
  *
  */
case class ViewUnit(size: Point[Int]) {
  private[this] val field = Field(size)
  private[this] var state = field.initState

  def view = state.view

  def trans(s: State => State) = state = s(state)

  def left() = field.moveLeft

  def right() = field.moveRight

  def rotate() = field.rotateCW

  def down() = field.moveDown

  def status() = field.transStatus

}
