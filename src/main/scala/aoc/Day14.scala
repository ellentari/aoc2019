package aoc

import aoc.util.Resources

import scala.annotation.tailrec
import scala.collection.immutable.Map

object Day14 extends App {

  type Chemical = String
  type Units    = Long

  case class Reaction(output: (Chemical, Units), input: List[(Chemical, Units)])

  case class ToDo(chemical: Chemical, units: Units, parent: Option[ToDo])
  case class State(totalOres: Units, produced: Map[Option[ToDo], Map[Chemical, Units]], leftovers: Map[Chemical, Units])

  case class ReactionResult(times: Int, produced: Units, extra: Units)

  private def parseReactions(input: List[String]): Map[Chemical, Reaction] = {

    def parseChemicalUnits(s: String): (Chemical, Units) = {
      val regex = "(\\d+) (\\w+)".r
      s match {
        case regex(q, e) => (e, q.toInt)
      }
    }

    def parseReaction(s: String): Reaction =
      s.split("\\s*=>\\s*") match {
        case Array(left, right) =>
          val input  = left.split(", ") map parseChemicalUnits
          val output = parseChemicalUnits(right)

          Reaction(output, input.toList)
      }

    input
      .map(parseReaction)
      .map(r => (r.output._1, r))
      .toMap
  }

  private def findRequiredOres(reactions: Map[Chemical, Reaction], fuelUnits: Units): Units = {

    def calculateReactionResult(toProduce: Units, output: Units): ReactionResult = {
      val times         = (toProduce.toDouble / output).ceil.toInt
      val producedUnits = times * output
      val extra         = producedUnits - toProduce

      ReactionResult(times, producedUnits, extra)
    }

    def addProduced(produced: Map[Option[ToDo], Map[Chemical, Units]], toDo: ToDo) = {
      val producedForParent = produced.getOrElse(toDo.parent, Map())

      produced.updated(toDo.parent, producedForParent.updated(toDo.chemical, toDo.units))
    }

    def tryTakeFromLeftovers(state: State, toDo: ToDo): Option[State] = {
      val left        = state.leftovers.getOrElse(toDo.chemical, 0L)
      val stillNeeded = toDo.units - left

      if (stillNeeded <= 0)
        Some(
          state.copy(
            produced = addProduced(state.produced, toDo),
            leftovers = state.leftovers.updated(toDo.chemical, left - toDo.units)
          )
        )
      else None
    }

    def produceSimple(state: State, toDo: ToDo, outputUnits: Units, ores: Units): State = {
      val left      = state.leftovers.getOrElse(toDo.chemical, 0L)
      val toProduce = toDo.units - left

      val result       = calculateReactionResult(toProduce, outputUnits)
      val oresRequired = result.times * ores

      State(
        totalOres = state.totalOres + oresRequired,
        produced = addProduced(state.produced, toDo),
        leftovers = state.leftovers.updated(toDo.chemical, result.extra)
      )
    }

    def produceComplex(state: State, toDo: ToDo, reaction: Reaction): Either[List[ToDo], State] = {
      val left            = state.leftovers.getOrElse(toDo.chemical, 0L)
      val toProduce       = toDo.units - left
      val alreadyProduced = state.produced.getOrElse(Some(toDo), Map()).withDefaultValue(0L)

      val result = calculateReactionResult(toProduce, reaction.output._2)

      val notDoneYet = reaction.input.map {
        case (chemical, units) => ToDo(chemical, units * result.times, Some(toDo))
      }.filter(todo => alreadyProduced(todo.chemical) < todo.units)

      if (notDoneYet.isEmpty)
        Right(
          state.copy(
            produced = addProduced(state.produced, toDo) - Some(toDo),
            leftovers = state.leftovers.updated(toDo.chemical, result.extra)
          )
        )
      else Left(notDoneYet)
    }

    @tailrec
    def loop(stack: List[ToDo], state: State): State =
      stack match {
        case Nil => state
        case toDo :: tail =>
          tryTakeFromLeftovers(state, toDo) match {
            case Some(nextState) => loop(tail, nextState)
            case _ =>
              reactions(toDo.chemical) match {
                case Reaction((_, outputUnits), ("ORE", oreUnits) :: Nil) =>
                  val nextState = produceSimple(state, toDo, outputUnits, oreUnits)
                  loop(tail, nextState)
                case complex =>
                  produceComplex(state, toDo, complex) match {
                    case Left(notDoneYet) => loop(notDoneYet ++ stack, state)
                    case Right(nextState) => loop(tail, nextState)
                  }
              }
          }
      }

    val initialState = State(0L, Map(), Map())
    val initialStack = List(ToDo("FUEL", fuelUnits, None))

    loop(initialStack, initialState).totalOres
  }

  private def findMaxFuelAmount(reactions: Map[Chemical, Reaction], maxOres: Units): Units = {
    @tailrec
    def binarySearch(lower: Units, upper: Units): Units =
      if (lower >= upper) upper
      else {
        val fuel = lower + (upper - lower) / 2
        val ores = findRequiredOres(reactions, fuel)

        if (ores < maxOres) binarySearch(fuel + 1, upper)
        else if (ores > maxOres) binarySearch(lower, fuel - 1)
        else fuel
      }

    binarySearch(lower = 1L, upper = maxOres)
  }

  def part1(reactions: Map[Chemical, Reaction]): Units =
    findRequiredOres(reactions, 1L)

  def part2(reactions: Map[Chemical, Reaction]): Units =
    findMaxFuelAmount(reactions, 1_000_000_000_000L)

  private val input = parseReactions(Resources.lines("day14.txt"))

  println(part1(input))
  println(part2(input))
}
