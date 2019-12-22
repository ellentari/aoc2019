package aoc

object Data {

  type Grid[A]  = Map[Point, A]

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

  def makeGrid[A](input: Seq[String])(mkCell: Char => A): Grid[A] = {
    (for {
      (row, y) <- input.view.zipWithIndex
      (ch, x)  <- row.view.zipWithIndex
      point    = Point(x, y)
      cell     = mkCell(ch)
    } yield (point, cell)).toMap
  }

  implicit class GridOps[A](val grid: Grid[A]) extends AnyVal {

    def adjacent(p: Point): List[Point] =
      Iterator((0, -1), (-1, 0), (0, 1), (1, 0))
        .map(p + _)
        .toList
  }

  implicit class PointOps(val p: Point) extends AnyVal {
    def +(d: (Int, Int)): Point =
      p.copy(x = p.x + d._1, y = p.y + d._2)
  }

}
