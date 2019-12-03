package aoc

import scala.annotation.tailrec

object Day3 extends App {

  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  case class Move(d: Int, direction: Direction)

  case class Point(x: Int, y: Int)

  def parse(input: List[String]): (List[Move], List[Move]) = {
    val right = "R(\\d+)".r
    val left = "L(\\d+)".r
    val up = "U(\\d+)".r
    val down = "D(\\d+)".r

    def parseMoves(path: String) = {
      (path.split(",") map {
        case right(d) => Move(d.toInt, Right)
        case left(d) => Move(d.toInt, Left)
        case up(d) => Move(d.toInt, Up)
        case down(d) => Move(d.toInt, Down)
      }).toList
    }

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
        val next = point.copy(y = point.y + d)
        val moved = fillY(point.x, point.y, next.y)
        (next, moved)
      case Move(d, Down) =>
        val next = point.copy(y = point.y - d)
        val moved = fillY(point.x, point.y, next.y)
        (next, moved)
      case Move(d, Right) =>
        val next = point.copy(x = point.x + d)
        val moved = fillX(point.y, point.x, next.x)
        (next, moved)
      case Move(d, Left) =>
        val next = point.copy(x = point.x - d)
        val moved = fillX(point.y, point.x, next.x)
        (next, moved)
    }
  }

  def distance(p1: Point, p2: Point): Int =
    (p2.x - p1.x).abs + (p2.y - p1.y).abs

  def part1(input: List[String]): Int = {

    def makeMoves(moves: List[Move]): Set[Point] = {

      @tailrec
      def loop(current: Point, moves: List[Move], acc: Set[Point]): Set[Point] = moves match {
        case Nil => acc
        case move :: rest =>
          val (next, visited) = makeMove(current, move)

          loop(next, rest, acc ++ visited)
      }

      loop(Point(0, 0), moves, Set())
    }

    val (moves1, moves2) = parse(input)

    val visited1 = makeMoves(moves1)
    val visited2 = makeMoves(moves2)

    val intersection = (visited1 intersect visited2) - Point(0, 0)

    intersection.map(distance(Point(0, 0), _)).min
  }

  def part2(input: List[String]): Int = {

    def makeMoves(moves: List[Move]): Map[Point, Int] = {

      @tailrec
      def loop(current: Point, steps: Int, path: List[Move], acc: Map[Point, Int]): Map[Point, Int] = path match {
        case Nil => acc
        case move :: rest =>
          val (next, visited) = makeMove(current, move)
          val withSteps = visited withFilter (!acc.contains(_)) map (p => (p, steps + distance(p, current)))

          loop(next, steps + move.d, rest, acc ++ withSteps)
      }

      loop(Point(0, 0), 0, moves, Map())
    }

    val (moves1, moves2) = parse(input)

    val visited1 = makeMoves(moves1)
    val visited2 = makeMoves(moves2)

    val intersection = (visited1.keySet intersect visited2.keySet) - Point(0, 0)

    intersection.map(p => visited1(p) + visited2(p)).min
  }

  private val input = Resources.lines("day3.txt")

  println(part1(input))
  println(part2(input))

}
