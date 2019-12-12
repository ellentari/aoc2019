package aoc

import aoc.Data.Point
import aoc.util.Resources

import scala.annotation.tailrec

object Day10 extends App {

  sealed trait Half
  case object Right extends Half
  case object Left  extends Half

  sealed trait Slope
  case class Defined(value: Double) extends Slope
  case object Undefined             extends Slope

  implicit val halfOrdering: Ordering[Half] = Ordering.by {
    case Right => 0
    case Left  => 1
  }

  implicit val slopeOrdering: Ordering[Slope] =
    (x: Slope, y: Slope) =>
      (x, y) match {
        case (Undefined, Defined(_))    => -1
        case (Defined(_), Undefined)    => 1
        case (Defined(d1), Defined(d2)) => Ordering[Double].reverse.compare(d1, d2)
      }

  def asteroids(input: Seq[String]): List[Point] = {
    val grid = input.map(_.toCharArray).toArray

    (for {
      y <- 0 until input.length
      x <- 0 until input(y).length if grid(y)(x) == '#'
    } yield Point(x, y)).toList
  }

  def eliminate(groups: Iterable[Seq[Point]]): List[Point] = {

    @tailrec
    def loop(remaining: Iterable[Seq[Point]], acc: Vector[Point]): Vector[Point] = {
      val (heads, tails) = remaining
        .map(ps => (ps.head, ps.tail))
        .unzip

      if (heads.isEmpty) acc
      else loop(tails.filter(_.nonEmpty), acc ++ heads)
    }

    loop(groups, Vector()).toList
  }

  def eliminate(asteroids: List[Point], location: Point): List[Point] = {
    val groups = slopes(location, asteroids).toSeq
      .groupMap(_._2)(_._1)
      .toSeq
      .sortBy(_._1)
      .map {
        case ((Right, _), points) => points.sortBy(p => (p.x, -p.y))
        case ((Left, _), points)  => points.sortBy(p => (-p.x, -p.y))
      }

    eliminate(groups)
  }

  def slopes(location: Point, asteroids: List[Point]): Map[Point, (Half, Slope)] = {
    def relative(base: Point, point: Point) =
      Point(point.x - base.x, base.y - point.y)

    def slope(rel: Point) =
      if (rel.x != 0) Defined(rel.y.toDouble / rel.x)
      else Undefined

    def half(rel: Point) =
      if (rel.x < 0) Left
      else if (rel.x > 0) Right
      else if (rel.y > 0) Right
      else Left

    asteroids
      .withFilter(_ != location)
      .map(p => {
        val rel = relative(location, p)

        (p, (half(rel), slope(rel)))
      })
      .toMap
  }

  def part1(asteroids: List[Point]): (Point, Int) =
    asteroids
      .map(a => (a, slopes(a, asteroids).values.toSet.size))
      .maxBy(_._2)

  def part2(asteroids: List[Point], location: Point, nth: Int): (Point, Int) = {
    val p = eliminate(asteroids, location).drop(nth - 1).head

    (p, p.x * 100 + p.y)
  }

  private val input = asteroids(Resources.lines("day10.txt"))

  private val part1Result = part1(input)

  println(part1Result._2)
  println(part2(input, part1Result._1, 200)._2)

}
