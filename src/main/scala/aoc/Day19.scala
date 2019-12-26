package aoc

import aoc.util.Data.Point
import aoc.util.IntcodeComputer._
import aoc.util.Resources

import scala.collection.Iterator.{iterate, unfold}

object Day19 extends App {

  sealed trait Cell
  case object Beam  extends Cell
  case object Empty extends Cell

  def isBeam(program: Memory, point: Point): Boolean = {
    def getCell: Cell = {
      val (_, output) = runProgram(programInput(point.x, point.y), ProgramState(program))

      output.head match {
        case 0 => Empty
        case 1 => Beam
      }
    }

    getCell == Beam
  }

  def part1(program: Memory, size: Int): Int =
    beamStartingPoints(program)
      .takeWhile(_.y < size)
      .flatMap(start => {
        iterate(start.x)(_ + 1)
          .map(Point(_, start.y))
          .takeWhile(isBeam(program, _))
          .filter(_.x < size)
      })
      .size

  def beamStartingPoints(program: Memory): Iterator[Point] =
    unfold(Point(0, -1)) { lastStart =>
      val y = lastStart.y + 1
      val beamStart = (lastStart.x to lastStart.x + 5)
        .collectFirst {
          case x if isBeam(program, Point(x, y)) => Point(x, y)
        }

      Some((beamStart, beamStart.getOrElse(Point(0, y))))
    }
    .flatten

  def part2(program: Memory, size: Int): Int = {
    def topRight(p: Point): Point =
      Point(p.x + (size - 1), p.y - (size - 1))

    def topLeft(p: Point): Point =
      p.copy(y = p.y - (size - 1))

    beamStartingPoints(program)
      .drop(size)
      .collectFirst {
        case p if isBeam(program, topRight(p)) => topLeft(p)
      }
      .map(p => p.x * 10000 + p.y)
      .get
  }

  private val input = parseMemory(Resources.string("day19.txt"))

  println(part1(input, 50))
  println(part2(input, 100))
}
