package aoc

import aoc.Search.runBFS
import aoc.util.IntcodeComputer._
import aoc.util.{AsciiComputer, Resources}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day25 extends App {

  type Path = List[Direction]

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object West  extends Direction
  case object East  extends Direction

  case class Room(name: String)
  case class Item(value: String)

  case class ProgramOutput(current: Location, seenSecured: Option[Location], raw: String)
  case class Location(room: Room, doorDirections: List[Direction], items: List[Item])

  case class GameMap(rooms: RoomMap, items: Map[Item, Room], secured: Room)

  case class RoomMap(graph: Map[Room, Map[Direction, Room]] = Map()) {
    def getAdjacent(room: Room): List[(Direction, Room)] =
      graph.getOrElse(room, Map()).toList

    def getAdjacentRoom(from: Room, direction: Direction): Option[Room] =
      getAdjacent(from).find(_._1 == direction).map(_._2)

    def getAdjacentRoomDirection(from: Room, to: Room): Option[Direction] =
      getAdjacent(from).find(_._2 == to).map(_._1)

    def exists(room: Room, direction: Direction): Boolean =
      getAdjacentRoom(room, direction).isDefined

    def updated(a: Room, direction: Direction, b: Room): RoomMap = {
      def addDoor(roomA: Room, direction: Direction, roomB: Room): Map[Direction, Room] =
        graph.getOrElse(a, Map()) + (direction -> b)

      RoomMap(graph + (a -> addDoor(a, direction, b)) + (b -> addDoor(b, backwards(direction), a)))
    }
  }

  sealed trait Instruction
  case class Go(direction: Direction) extends Instruction
  case class Take(item: Item)         extends Instruction
  case class Drop(item: Item)         extends Instruction

  def runProgram(input: Option[Instruction], programState: ProgramState): (ProgramState, ProgramOutput) = {
    def instructionToString(i: Instruction): String = i match {
      case Go(direction) => direction.toString.toLowerCase
      case Take(item)    => s"take ${item.value}"
      case Drop(item)    => s"drop ${item.value}"
    }

    def getList(listStart: String, lines: Seq[String]): List[String] =
      lines.view
        .dropWhile(_ != listStart)
        .drop(1)
        .takeWhile(_.startsWith("-"))
        .map(_.substring(2))
        .toList

    def parseDirection(d: String): Direction = d match {
      case "north" => North
      case "south" => South
      case "east"  => East
      case "west"  => West
    }

    def parseLocation(location: String): Location = {
      val lines = location.split("\n").filter(_.trim.nonEmpty)

      val room  = lines.head.stripPrefix("== ").stripSuffix(" ==")
      val doors = getList("Doors here lead:", lines).map(parseDirection)
      val items = getList("Items here:", lines).map(Item)

      Location(Room(room), doors, items)
    }

    def parseProgramOutput(output: String): ProgramOutput = {
      val allLocations = output
        .split("== ")
        .filter(_.trim.nonEmpty)
        .map(parseLocation)
        .reverse
        .toList

      val finalLocation = allLocations.head
      val secured       = allLocations.tail.headOption

      ProgramOutput(finalLocation, secured, output)
    }

    val (nextState, output) = AsciiComputer.runProgramToString(programState, input.map(instructionToString))

    (nextState, parseProgramOutput(output))
  }

  def backwards(here: Direction): Direction = here match {
    case North => South
    case South => North
    case East  => West
    case West  => East
  }

  def discoverGameMap(program: Memory): GameMap = {
    case class Move(from: Location, direction: Direction, program: ProgramState)

    def updateRoomMap(rooms: RoomMap, from: Room, to: Room, direction: Direction, secured: Option[Room]): RoomMap =
      if (from != to)
        rooms.updated(from, direction, to)
      else
        secured.map(rooms.updated(from, direction, _)).getOrElse(rooms)

    def nextMoves(from: Location, programState: ProgramState): List[Move] =
      from.doorDirections
        .map(d => Move(from, d, programState))

    @tailrec
    def loop(queue: Queue[Move], graph: RoomMap, items: Map[Item, Room], seenSecured: Option[Location]): GameMap = {
      val (Move(from, direction, programState), tail) = queue.dequeue

      val (nextProgramState, ProgramOutput(current, secured, _)) = runProgram(Some(Go(direction)), programState)

      val updatedItems = items ++ current.items.map(_ -> current.room).toMap
      val updatedGraph = updateRoomMap(graph, from.room, current.room, direction, secured.map(_.room))
      val updatedQueue = tail ++ nextMoves(current, nextProgramState)
        .filterNot(m => updatedGraph.exists(m.from.room, m.direction))

      if (updatedQueue.isEmpty)
        GameMap(updatedGraph, updatedItems, seenSecured.orElse(secured).get.room)
      else
        loop(updatedQueue, updatedGraph, updatedItems, seenSecured.orElse(secured))
    }

    val (programState, ProgramOutput(location, _, _)) = runProgram(None, ProgramState(program))
    val initial                                       = nextMoves(location, programState)

    loop(Queue.from(initial), RoomMap(), Map(), None)
  }

  def findPath(from: Room, to: Room, graph: RoomMap): Path = {
    case class Segment(parent: Option[Segment], direction: Direction, room: Room)

    def getAdjacent(room: Room, parent: Option[Segment]): List[Segment] =
      graph
        .getAdjacent(room)
        .map(door => Segment(parent, door._1, door._2))

    @tailrec
    def recreatePath(segment: Segment, acc: Path): Path = {
      val next = segment.direction :: acc
      segment.parent match {
        case Some(parent) => recreatePath(parent, next)
        case None         => next
      }
    }

    if (from == to) Nil
    else {
      val initial = getAdjacent(from, None)
      val path    = runBFS(initial)(p => getAdjacent(p.room, Some(p)))(_.room == to)(_.room).toOption.get

      recreatePath(path, Nil)
    }
  }

  def takePath(path: Path, programState: ProgramState) =
    path
      .map(Go)
      .foldLeft(programState)((state, go) => runProgram(Some(go), state)._1)

  def doWithItems(items: Iterable[Item], toInstruction: Item => Instruction, programState: ProgramState): ProgramState =
    items
      .map(toInstruction)
      .foldLeft(programState) {
        case (state, instruction) => runProgram(Some(instruction), state)._1
      }

  def takeItems(items: Iterable[Item], programState: ProgramState): ProgramState =
    doWithItems(items, Take, programState)

  def collectItems(rooms: RoomMap, itemLocations: Map[Item, Room], program: Memory): (Set[Item], Room, ProgramState) = {
    val toCollect = itemLocations.filterNot(ir => itemBlackList.contains(ir._1.value)).keySet

    def getPathsToItems(from: Room, items: Set[Item]): Map[Item, Path] =
      items
        .map(item => (item, findPath(from, itemLocations(item), rooms)))
        .toMap

    @tailrec
    def loop(current: Room, collected: Set[Item], programState: ProgramState): (Room, ProgramState) = {
      val remaining = toCollect -- collected

      if (remaining.isEmpty) (current, programState)
      else {
        val pathsToItems = getPathsToItems(current, remaining)

        val (closestItem, shortestPath) = pathsToItems.minBy(_._2.length)
        val closestItemRoom             = itemLocations(closestItem)

        val stateAfterMove    = takePath(shortestPath, programState)
        val stateAfterCollect = takeItems(List(closestItem), stateAfterMove)

        loop(closestItemRoom, collected + closestItem, stateAfterCollect)
      }
    }

    val (programState, ProgramOutput(location, _, _)) = runProgram(None, ProgramState(program))

    val (finalRoom, finalProgramState) = loop(location.room, Set(), programState)

    (toCollect, finalRoom, finalProgramState)
  }

  def findPasscode(current: Room, items: Set[Item], secured: Room, rooms: RoomMap, programState: ProgramState): ProgramOutput = {
    val securityCheckpoint = rooms.getAdjacent(secured).head._2
    val pathToCheckpoint   = findPath(current, securityCheckpoint, rooms)

    val atCheckpoint = takePath(pathToCheckpoint, programState)

    enterSecured(securityCheckpoint, secured, rooms, items.toList, atCheckpoint)
  }

  def enterSecured(from: Room, target: Room, graph: RoomMap, items: List[Item], programState: ProgramState): ProgramOutput = {
    def dropItems(items: Iterable[Item], programState: ProgramState): ProgramState =
      doWithItems(items, Drop, programState)

    def tryEnter(programState: ProgramState): Option[ProgramOutput] = {
      val direction = graph.getAdjacentRoomDirection(from, target).get
      val (_, out)  = runProgram(Some(Go(direction)), programState)

      if (out.current.room != from) Some(out)
      else None
    }

    val stateAfterDrop = dropItems(items, programState)

    (1 to items.size).view
      .flatMap(items.combinations)
      .map(takeItems(_, stateAfterDrop))
      .flatMap(tryEnter)
      .head
  }

  val itemBlackList = Set(
    "molten lava",
    "infinite loop",
    "photons",
    "escape pod",
    "giant electromagnet"
  )

  def part1(input: Memory): String = {
    val GameMap(rooms, items, securedRoom)   = discoverGameMap(input)
    val (collectedItems, room, programState) = collectItems(rooms, items, input)

    val finalOutput = findPasscode(room, collectedItems, securedRoom, rooms, programState)

    finalOutput.raw
  }

  private val input = parseMemory(Resources.string("day25.txt"))

  println(part1(input))
}
