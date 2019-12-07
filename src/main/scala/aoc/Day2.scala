package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day2 extends App {

  type Address = Int
  type Memory  = IndexedSeq[Address]

  case class Params(left: Address, right: Address, result: Address)
  case class Instruction(opCode: Int, params: Option[Params])

  def parseInput(s: String): Memory =
    s.split(",").map(_.toInt)

  def part1(input: Memory): Int =
    run(input, 12, 2)

  def part2(input: Memory): Int = {
    val (noun, verb) = (for {
      noun <- 0 to 99
      verb <- 0 to 99 if run(input, noun, verb) == 19690720
    } yield (noun, verb)).head

    100 * noun + verb
  }

  def run(initialMemory: Memory, noun: Int, verb: Int): Int = {

    def getInstruction(memory: Memory, pointer: Address): Option[(Instruction, Address)] = {
      def params =
        for {
          p1 <- memory.lift(pointer + 1)
          p2 <- memory.lift(pointer + 2)
          p3 <- memory.lift(pointer + 3)
        } yield Params(p1, p2, p3)

      memory
        .lift(pointer)
        .map(opCode => (Instruction(opCode, params), pointer + 4))
    }

    def process(memory: Memory, instruction: Instruction): Option[Memory] = instruction match {
      case Instruction(99, _) => None
      case Instruction(1, Some(Params(left, right, result))) =>
        Some(memory.updated(result, memory(left) + memory(right)))
      case Instruction(2, Some(Params(left, right, result))) =>
        Some(memory.updated(result, memory(left) * memory(right)))
    }

    @tailrec
    def loop(memory: Memory, pointer: Address): Memory = getInstruction(memory, pointer) match {
      case None => memory
      case Some((instruction, nextPointer)) =>
        process(memory, instruction) match {
          case Some(nextMemory) => loop(nextMemory, nextPointer)
          case None             => memory
        }
    }

    val memory = initialMemory.updated(1, noun).updated(2, verb)
    val result = loop(memory, 0)
    result(0)
  }

  private val input = parseInput(Resources.string("day2.txt"))

  println(part1(input))
  println(part2(input))
}
