package aoc

import Data._
import aoc.util.IntcodeComputer._
import aoc.util.{IntcodeComputer, Resources}

import scala.annotation.tailrec

object Day11 extends App {

  sealed trait Color
  case object Black extends Color
  case object White extends Color

  def part1(program: Memory): Int = {
    val painted = paint(program, Black)

    painted.size
  }

  def part2(program: Memory): List[String] = {
    val painted = paint(program, White)

    draw(painted)
  }

  private def paint(program: Memory, initial: Color): Map[Point, Color] = {
    def turn(current: Direction, turn: Direction) = turn match {
      case Left =>
        current match {
          case Up    => Left
          case Left  => Down
          case Down  => Right
          case Right => Up
        }
      case Right =>
        current match {
          case Up    => Right
          case Right => Down
          case Down  => Left
          case Left  => Up
        }
    }

    @tailrec
    def loop(point: Point, direction: Direction, grid: Map[Point, Color], state: ProgramState): Map[Point, Color] = {
      val color = grid.getOrElse(point, Black)

      val input = color match {
        case Black => 0
        case White => 1
      }

      val (programReturn, first :: second :: Nil) = runProgram(programInput(input), state)

      val colorToPaint = first match {
        case 0 => Black
        case 1 => White
      }

      val toTurn = second match {
        case 0 => Left
        case 1 => Right
      }

      val nextGrid = grid.updated(point, colorToPaint)

      val nextDirection = turn(direction, toTurn)
      val nextPoint     = movePoint(point, nextDirection, 1)

      programReturn.code match {
        case Exit  => grid
        case Block => loop(nextPoint, nextDirection, nextGrid, programReturn.state)
      }
    }

    val startingPosition = Point(0, 0)
    val initialGrid      = Map(startingPosition -> initial)

    loop(startingPosition, Up, initialGrid, ProgramState(program))
  }

  private def draw(colors: Map[Point, Color]): List[String] = {
    def minMax(iterable: Iterable[Int]): (Int, Int) =
      (iterable.min, iterable.max)

    val (minX, maxX) = minMax(colors.keys.map(_.x))
    val (minY, maxY) = minMax(colors.keys.map(_.y))

    val grid = Array.fill(minY.abs + maxY.abs + 1, minX.abs + maxX.abs + 1)(' ')

    colors.foreach {
      case (point, color) =>
        val i = maxY - point.y
        val j = minX.abs + point.x

        grid(i)(j) = color match {
          case Black => ' '
          case White => '#'
        }
    }

    grid.map(chars => new String(chars)).toList
  }

  private val input = IntcodeComputer.parseMemory(Resources.string("day11.txt"))

  println(part1(input))
  println(part2(input).mkString("\n"))
}
