package aoc

import aoc.util.Data._
import aoc.util.{Resources, Search}

object Day20 extends App {

  type Maze  = Grid[Cell]
  type Gates = Map[String, (Gate, Gate)]

  type Level        = Int
  type GetNextLevel = (Gate, Level) => Option[Level]

  sealed trait MazeSide
  case object Outer extends MazeSide
  case object Inner extends MazeSide

  sealed trait Cell
  case class Entry(point: Point)                             extends Cell
  case class Exit(point: Point)                              extends Cell
  case class Gate(name: String, side: MazeSide, exit: Point) extends Cell
  case object Empty                                          extends Cell
  case object Nothing                                        extends Cell
  case object Wall                                           extends Cell

  private def readMaze(input: IndexedSeq[String]): Maze = {
    def isNamePart(char: Char): Boolean =
      char.isLetter && char.isUpper

    def mazeSide(point: Point): MazeSide = point match {
      case Point(x, y) if x <= 2 || y <= 2                                  => Outer
      case Point(x, y) if x >= input(y).length - 2 || y >= input.length - 2 => Outer
      case _                                                                => Inner
    }

    def getNamedCell(p: Point): Option[Cell] = {
      val nameAndDirection = Iterator((0, -1), (-1, 0), (0, 1), (1, 0))
        .map(
          d =>
            List(
              p + (-d._1, -d._2),
              p,
              p + d,
              p + d + d
            )
        )
        .map(points => points.map(p => (p, input.lift(p.y).flatMap(row => row.lift(p.x)).getOrElse(' '))))
        .collectFirst {
          case (p, '.') :: (_, b) :: (_, c) :: (_, ' ') :: Nil if isNamePart(b) && isNamePart(c) =>
            (new String(Array(b, c).sorted), p)
        }

      nameAndDirection match {
        case Some(("AA", point)) => Some(Entry(point))
        case Some(("ZZ", point)) => Some(Exit(point))
        case Some((name, exit))  => Some(Gate(name, mazeSide(p), exit))
        case _                   => None
      }
    }

    def getCell(char: Char, point: Point): Option[Cell] =
      char match {
        case '#'                => Some(Wall)
        case '.'                => Some(Empty)
        case a if isNamePart(a) => getNamedCell(point)
        case _                  => Some(Nothing)
      }

    (for {
      (row, y)  <- input.view.zipWithIndex
      (char, x) <- row.view.zipWithIndex
      point     = Point(x, y)
      cell      <- getCell(char, point)
    } yield (point, cell)).toMap
  }

  private def findPathToExit(input: List[String])(getNextLevel: GetNextLevel): Int = {
    def findGates(maze: Maze): Map[String, (Gate, Gate)] =
      maze.view
        .collect[Gate] { case (_, c: Gate) => c }
        .groupBy(_.name)
        .view
        .mapValues(gates => (gates.head, gates.tail.head))
        .toMap

    def findEntry(maze: Maze): Point =
      maze.collectFirst {
        case (p, e: Entry) => (p, e)
      }.get._2.point

    def findExitPoint(maze: Maze): Point =
      maze.collectFirst {
        case (p, e: Exit) => (p, e)
      }.get._2.point

    val maze  = readMaze(input.toIndexedSeq)
    val gates = findGates(maze)

    findPath(findEntry(maze), findExitPoint(maze), maze, gates)(getNextLevel).get
  }

  private def findPath(start: Point, end: Point, maze: Maze, gates: Gates)(getNextLevel: GetNextLevel): Option[Int] = {

    case class Move(parent: Option[Move], point: Point, steps: Int, level: Level = 0)

    def passGate(from: Move, entryGate: Gate): Option[Move] =
      getNextLevel(entryGate, from.level)
        .map(nextLevel => {
          val (gate1, gate2) = gates(entryGate.name)
          val exitGate       = List(gate1, gate2).find(_ != entryGate).get

          Move(Some(from), exitGate.exit, from.steps + 1, nextLevel)
        })

    def nextMove(to: Point, after: Move): Option[Move] =
      maze.getOrElse(to, Nothing) match {
        case Empty      => Some(Move(Some(after), to, after.steps + 1, after.level))
        case gate: Gate => passGate(after, gate)
        case _ => None
      }

    def nextMoves(after: Move): List[Move] =
      maze
        .adjacent(after.point)
        .flatMap(point => nextMove(point, after))

    def exitFound(move: Move): Boolean =
      move.point == end && move.level == 0

    val initial = Move(None, start, 0)
    val result = Search.runBFS(List(initial))(nextMoves)(exitFound)(m => (m.level, m.point))
    result.toOption.map(_.steps)
  }

  def part1(input: List[String]): Int = {
    val nextLevel: GetNextLevel = (_, level) => Some(level)

    findPathToExit(input)(nextLevel)
  }

  def part2(input: List[String]): Int = {
    val nextLevel: GetNextLevel = {
      case (Gate(_, Outer, _), level) if level > 0 => Some(level - 1)
      case (Gate(_, Inner, _), level)              => Some(level + 1)
      case _                                       => None
    }

    findPathToExit(input)(nextLevel)
  }

  private val input = Resources.lines("day20.txt")

  println(part1(input))
  println(part2(input))

}
