package aoc

import aoc.Data._
import aoc.util.IntcodeComputer._
import aoc.util.{IntcodeComputer, Resources}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day15 extends App {

  type Steps   = Int
  type AreaMap = Map[Point, (Cell, Steps)]

  sealed trait Cell
  case object Wall         extends Cell
  case object Empty        extends Cell
  case object OxygenSystem extends Cell

  sealed trait DroidStatus
  case object HitWall           extends DroidStatus
  case object Moved             extends DroidStatus
  case object FoundOxygenSystem extends DroidStatus

  case class Move(point: Point, steps: Steps, direction: Direction)

  private def discoverAreaMap(memory: Memory): AreaMap = {
    def status2Cell(status: DroidStatus): Cell = status match {
      case HitWall           => Wall
      case Moved             => Empty
      case FoundOxygenSystem => OxygenSystem
    }

    def nextMoves(point: Point, steps: Steps, map: AreaMap): List[Move] =
      possibleMoves(point, steps)
        .filter(move => !map.contains(move.point))

    @tailrec
    def loop(queue: Queue[(Move, ProgramState)], map: AreaMap): Option[AreaMap] =
      queue.dequeueOption match {
        case None => None
        case Some(((Move(point, steps, direction), programState), tail)) =>
          if (map.contains(point)) loop(tail, map)
          else {
            val (status, updatedProgramState) = moveDroid(programState, direction)
            val updatedMap                    = map.updated(point, (status2Cell(status), steps))

            status match {
              case FoundOxygenSystem => Some(updatedMap)
              case HitWall           => loop(tail, updatedMap)
              case Moved =>
                val updatedQueue = tail ++ nextMoves(point, steps, updatedMap).map(m => (m, updatedProgramState))

                loop(updatedQueue, updatedMap)
            }
          }
      }

    val map          = Map(Point(0, 0) -> (Empty, 0))
    val programState = ProgramState(memory)
    val initialMoves = nextMoves(Point(0, 0), 0, map).map(m => (m, programState))

    loop(Queue.from(initialMoves), map).get
  }

  private def moveDroid(programState: ProgramState, direction: Direction): (DroidStatus, ProgramState) = {
    val input = direction match {
      case Up    => 1
      case Down  => 2
      case Left  => 3
      case Right => 4
    }

    val (Return(_, state), output :: Nil) =
      IntcodeComputer.runProgram(IntcodeComputer.programInput(input), programState)

    val status = output match {
      case 0 => HitWall
      case 1 => Moved
      case 2 => FoundOxygenSystem
    }

    (status, state)
  }

  private def possibleMoves(point: Point, stepsSoFar: Steps): List[Move] = {
    val singleStep = 1
    val moveSteps  = stepsSoFar + singleStep

    List(
      Move(movePoint(point, Up, singleStep), moveSteps, Up),
      Move(movePoint(point, Down, singleStep), moveSteps, Down),
      Move(movePoint(point, Left, singleStep), moveSteps, Left),
      Move(movePoint(point, Right, singleStep), moveSteps, Right)
    )
  }

  private def findOxygenSystem(map: AreaMap): (Point, Steps) = {
    val (point, (_, steps)) = map
      .find(_._2._1 == OxygenSystem)
      .get

    (point, steps)
  }

  def part1(memory: Memory): Steps = {
    val areaMap = discoverAreaMap(memory)

    findOxygenSystem(areaMap)._2
  }

  def part2(memory: Memory): Steps = {
    val areaMap = discoverAreaMap(memory)

    def nextMoves(point: Point, steps: Steps, seen: Set[Point]): List[Move] =
      possibleMoves(point, steps)
        .filter(move => !seen.contains(move.point))
        .filter(move => areaMap.get(move.point).map(_._1).exists(_ != Wall))

    @tailrec
    def loop(queue: Queue[Move], steps: Steps, seen: Set[Point]): Steps =
      queue.dequeueOption match {
        case None => steps
        case Some((Move(point, steps, _), tail)) =>
          if (seen.contains(point))
            loop(tail, steps, seen)
          else {
            val updatedSeen  = seen + point
            val updatedQueue = tail ++ nextMoves(point, steps, updatedSeen)

            loop(updatedQueue, steps, updatedSeen)
          }
      }

    val oxygenSystem = findOxygenSystem(areaMap)._1
    val seen         = Set(oxygenSystem)
    val initialMoves = nextMoves(oxygenSystem, 0, seen)

    loop(Queue.from(initialMoves), 0, seen)
  }

  private val input = parseMemory(Resources.string("day15.txt"))

  println(part1(input))
  println(part2(input))
}
