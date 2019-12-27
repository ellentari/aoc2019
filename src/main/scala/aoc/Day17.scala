package aoc

import aoc.util.Data._
import aoc.util.IntcodeComputer._
import aoc.util.{AsciiComputer, Data, Resources}

import scala.annotation.tailrec

object Day17 extends App {

  sealed trait Cell
  case class Robot(direction: Direction) extends Cell
  case object Scaffold                   extends Cell
  case object Empty                      extends Cell

  sealed trait Move
  case class Forward(n: Int) extends Move
  case object Right          extends Move
  case object Left           extends Move

  type AreaMap = Grid[Cell]
  type Path    = List[Move]

  private def readMap(memory: Memory): AreaMap = {
    val (_, output) = AsciiComputer.runProgramToString(ProgramState(memory), None)

    makeGrid(output.split("\n")) {
      case '#' => Scaffold
      case '.' => Empty
      case '^' => Robot(Up)
      case '>' => Robot(Data.Right)
      case '<' => Robot(Data.Left)
      case 'v' => Robot(Down)
    }
  }

  private def findRobot(map: AreaMap): Option[(Point, Robot)] =
    map.collectFirst {
      case (p, Robot(d)) => (p, Robot(d))
    }

  private def getRobotPath(map: AreaMap): Path = {

    case class Segment(p: Point, d: Direction, m: Move, stepsForward: Int)

    def tryMove(point: Point, direction: Direction): Option[(Point, Direction)] = {
      val next = point + (direction match {
        case Up => (0, -1)
        case Down => (0, 1)
        case Data.Left => (-1, 0)
        case Data.Right => (1, 0)
      })

      map
        .get(next)
        .filter(_ == Scaffold)
        .map(_ => (next, direction))
    }

    def nextSegment(point: Point, direction: Direction, stepsForward: Int): Option[(Point, Direction, Move, Int)] =
      tryMove(point, direction)
        .map(m => (m._1, m._2, Forward(stepsForward + 1), stepsForward + 1))
        .orElse(tryMove(point, turnLeft(direction)).map(m => (m._1, m._2, Left, 1)))
        .orElse(tryMove(point, turnRight(direction)).map(m => (m._1, m._2, Right, 1)))

    def addMove(move: Move, path: Path): Path = (move, path) match {
      case (Forward(n), Forward(_) :: rest) => Forward(n) :: rest
      case _                                => move :: path
    }

    @tailrec
    def loop(point: Point, direction: Direction, stepsForward: Int, path: Path): Path =
      nextSegment(point, direction, stepsForward) match {
        case None => path
        case Some((nextPoint, nextDir, move, nextStepsForward)) =>
          loop(nextPoint, nextDir, nextStepsForward, addMove(move, path))
      }

    val (point, robot) = findRobot(map).get

    loop(point, robot.direction, 0, Nil).reverse
  }

  private def findGroups(stringPath: String): (String, String, String) = {

    def findGroup(s: String): (String, String) = {
      val groupReplacement = "Z"

      @tailrec
      def longestRepeatingPrefix(last: String, prefix: String, path: String): String = {
        if (path.indexOf(prefix, 1) >= 0) {
          val firstComa  = path.indexOf(",", prefix.length + 1)
          val secondComa = path.indexOf(",", firstComa + 1)
          val nextPrefix = path.substring(0, secondComa)

          if (nextPrefix.contains(groupReplacement)) prefix
          else longestRepeatingPrefix(prefix, nextPrefix, path)
        }
        else last
      }

      @tailrec
      def removePrefix(s: String, prefix: String): String =
        if (s.startsWith(prefix)) removePrefix(s.stripPrefix(prefix), prefix)
        else s

      val group = longestRepeatingPrefix("", "", s)
      val next = removePrefix(s.replaceAll(group, groupReplacement),  groupReplacement + ",")

      (group, next)
    }

    val (a, pathLeft1) = findGroup(stringPath)
    val (b, pathLeft2) = findGroup(pathLeft1)
    val (c, _)         = findGroup(pathLeft2)

    (a, b, c)
  }

  def part1(input: Memory): Int = {
    def isIntersection(point: Point, map: AreaMap): Boolean =
      (point :: map.adjacent(point))
        .map(map.getOrElse(_, Empty))
        .forall(_ == Scaffold)

    val map             = readMap(input)
    val (robotPoint, _) = findRobot(map).get

    map.updated(robotPoint, Scaffold)
      .keys
      .filter(isIntersection(_, map))
      .map(p => p.x * p.y)
      .sum
  }

  def part2(program: Memory): Value = {

    def pathToString(path: Path): String =
      path.map {
        case Left       => "L"
        case Right      => "R"
        case Forward(n) => n.toString
      }.mkString(",")

    def splitInput(stringPath: String): (String, (String, String, String)) = {
      val (a, b, c) = findGroups(stringPath)

      val compressed = stringPath
        .replaceAll(a, "A")
        .replaceAll(b, "B")
        .replaceAll(c, "C")

      (compressed, (a, b, c))
    }

    def toProgramInput(path: Path): String = {
      val stringPath = pathToString(path)

      val (mainRoutine, (a, b, c)) = splitInput(stringPath)

      List(mainRoutine, a, b, c, "n").mkString("\n")
    }

    val map   = readMap(program)
    val path  = getRobotPath(map)
    val input = toProgramInput(path)

    val (_, output) = AsciiComputer.runProgram(ProgramState(program.updated(0L, 2)), Some(input))

    output.last
  }

  private val input = parseMemory(Resources.string("day17.txt"))

  println(part1(input))
  println(part2(input))

}
