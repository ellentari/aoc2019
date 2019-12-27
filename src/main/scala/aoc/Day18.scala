package aoc

import aoc.util.Data._
import aoc.util.{Resources, Search}

object Day18 extends App {

  sealed trait Cell
  case class Key(value: Char)  extends Cell
  case class Door(value: Char) extends Cell
  case object Wall             extends Cell
  case object Empty            extends Cell
  case object Entry            extends Cell

  type Maze         = Grid[Cell]
  type KeyLocations = Map[Key, Point]
  type Keys         = Set[Key]

  case class Move(point: Point, steps: Int, collected: Keys)

  def parseMaze(input: List[String]): Maze =
    makeGrid(input) {
      case '#'            => Wall
      case '.'            => Empty
      case '@'            => Entry
      case c if c.isLower => Key(c)
      case c if c.isUpper => Door(c.toLower)
    }

  private def findKeys(maze: Maze): KeyLocations =
    maze.collect {
      case (p, k: Key) => (k, p)
    }

  private def findEntry(maze: Maze): Point =
    maze.view.collectFirst {
      case (point, Entry) => point
    }.get

  private def fewestStepsToFindKeys(start: Point, initialKeys: Set[Key], maze: Maze, keys: KeyLocations): Int = {
    def nextMove(point: Point, steps: Int, collected: Keys): Option[Move] =
      maze.getOrElse(point, Wall) match {
        case Empty                                     => Some(Move(point, steps, collected))
        case key: Key                                  => Some(Move(point, steps, collected + key))
        case Door(key) if collected.contains(Key(key)) => Some(Move(point, steps, collected))
        case _                                         => None
      }

    def nextMoves(move: Move): List[Move] =
      maze
        .adjacent(move.point)
        .flatMap(point => nextMove(point, move.steps + 1, move.collected))

    def allKeysCollected(move: Move): Boolean =
      (keys.keySet -- move.collected).isEmpty

    val initial = Move(start, 0, initialKeys)

    Search.runBFS(List(initial))(nextMoves)(allKeysCollected)(m => (m.point, m.collected)).toOption.get.steps
  }

  private def reachableKeys(start: Point, maze: Maze, keys: KeyLocations): Set[Key] = {
    def allReachablePoints(start: Point): Set[Point] = {
      def nextMoves(move: Point): List[Point] =
        maze.adjacent(move)
          .flatMap(point => maze.getOrElse(point, Wall) match {
            case Empty   => Some(point)
            case Key(_)  => Some(point)
            case Door(_) => Some(point)
            case _       => None
          })

      Search.runBFS(List(start))(nextMoves)(_ => false)(identity).swap.toOption.get
    }

    val reachablePoints = allReachablePoints(start)
    val reachableKeys   = reachablePoints & keys.values.toSet

    keys.collect {
      case (key, point) if reachableKeys.contains(point) => key
    }.toSet
  }

  def part1(input: List[String]): Int = {
    val maze  = parseMaze(input)
    val keys  = findKeys(maze)
    val start = findEntry(maze)

    fewestStepsToFindKeys(start, Set(), maze + (start -> Empty), keys)
  }

  def part2(input: List[String]): Int = {
    def updateMazeWithNewEntries(maze: Maze): (List[Point], Maze) = {
      val entry = findEntry(maze)

      val newEntries = List((1, -1), (1, 1), (-1, 1), (-1, -1)) map (entry + _)
      val walls      = List((0, -1), (0, 1), (0, 0), (-1, 0), (1, 0)) map (entry + _)

      (newEntries, maze ++ newEntries.map(_ -> Empty) ++ walls.map(_ -> Wall))
    }

    def fewestSteps(entry: Point, maze: Maze, keys: KeyLocations) = {
      val reachable = reachableKeys(entry, maze, keys)
      fewestStepsToFindKeys(entry, keys.keySet -- reachable, maze, keys)
    }

    val (entries, maze) = updateMazeWithNewEntries(parseMaze(input))
    val keys            = findKeys(maze)

    entries.map(fewestSteps(_, maze, keys)).sum
  }

  private val input = Resources.lines("day18.txt")

  println(part1(input))
  println(part2(input))
}
