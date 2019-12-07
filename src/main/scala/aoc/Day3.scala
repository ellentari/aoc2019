package aoc

import aoc.util.Resources

object Day3 extends App {

  sealed trait Direction
  case object Up    extends Direction
  case object Down  extends Direction
  case object Left  extends Direction
  case object Right extends Direction

  case class Move(d: Int, direction: Direction)

  case class Point(x: Int, y: Int)

  def parse(input: List[String]): (List[Move], List[Move]) = {
    val right = "R(\\d+)".r
    val left  = "L(\\d+)".r
    val up    = "U(\\d+)".r
    val down  = "D(\\d+)".r

    def parseMoves(path: String) =
      path.split(",").map {
        case right(d) => Move(d.toInt, Right)
        case left(d)  => Move(d.toInt, Left)
        case up(d)    => Move(d.toInt, Up)
        case down(d)  => Move(d.toInt, Down)
      }.toList

    input match {
      case first :: second :: Nil =>
        (parseMoves(first), parseMoves(second))
    }
  }

  def makeMove(point: Point, move: Move): (Point, Seq[Point]) = {

    def fillX(y: Int, x1: Int, x2: Int) =
      ((x1 min x2) to (x1 max x2)) map (Point(_, y))

    def fillY(x: Int, y1: Int, y2: Int) =
      ((y1 min y2) to (y1 max y2)) map (Point(x, _))

    move match {
      case Move(d, Up) =>
        val next  = point.copy(y = point.y + d)
        val moved = fillY(point.x, point.y, next.y)
        (next, moved)
      case Move(d, Down) =>
        val next  = point.copy(y = point.y - d)
        val moved = fillY(point.x, point.y, next.y)
        (next, moved)
      case Move(d, Right) =>
        val next  = point.copy(x = point.x + d)
        val moved = fillX(point.y, point.x, next.x)
        (next, moved)
      case Move(d, Left) =>
        val next  = point.copy(x = point.x - d)
        val moved = fillX(point.y, point.x, next.x)
        (next, moved)
    }
  }

  def distance(p1: Point, p2: Point): Int =
    (p2.x - p1.x).abs + (p2.y - p1.y).abs

  def part1(path1: List[Move], path2: List[Move]): Int = {

    def makeMoves(path: List[Move]): Set[Point] = {
      path.foldLeft((Point(0, 0), Set.empty[Point])) {
        case ((current, seen), move) =>
          val (next, newSeen) = makeMove(current, move)

          (next, seen ++ newSeen)
      }._2
    }

    val visited1 = makeMoves(path1)
    val visited2 = makeMoves(path2)

    val intersection = (visited1 intersect visited2) - Point(0, 0)

    intersection.map(distance(Point(0, 0), _)).min
  }

  def part2(path1: List[Move], path2: List[Move]): Int = {

    def makeMoves(path: List[Move]): Map[Point, Int] = {
      path.foldLeft((Point(0, 0), 0, Map.empty[Point, Int])) {
        case ((current, steps, seen), move) =>
          val (next, newSeen) = makeMove(current, move)
          val withSteps = newSeen.withFilter(!seen.contains(_)).map(p => (p, steps + distance(p, current)))

          (next, steps + move.d, seen ++ withSteps)
      }._3
    }

    val visited1 = makeMoves(path1)
    val visited2 = makeMoves(path2)

    val intersection = (visited1.keySet intersect visited2.keySet) - Point(0, 0)

    intersection.map(p => visited1(p) + visited2(p)).min
  }

  private val (path1, path2) = parse(Resources.lines("day3.txt"))

  println(part1(path1, path2))
  println(part2(path1, path2))
}
