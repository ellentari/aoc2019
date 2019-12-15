package aoc

object Data {

  case class Point(x: Int, y: Int)

  sealed trait Direction
  case object Up    extends Direction
  case object Down  extends Direction
  case object Left  extends Direction
  case object Right extends Direction

  def movePoint(point: Point, direction: Direction, d: Int): Point = direction match {
    case Up    => point.copy(y = point.y + d)
    case Down  => point.copy(y = point.y - d)
    case Left  => point.copy(x = point.x - d)
    case Right => point.copy(x = point.x + d)
  }
}
