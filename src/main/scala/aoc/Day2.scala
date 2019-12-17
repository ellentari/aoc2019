package aoc

import aoc.util.IntcodeComputer._
import aoc.util.{IntcodeComputer, Resources}

object Day2 extends App {

  def part1(input: Memory): Value =
    run(input, 12L, 2L)

  def part2(input: Memory): Value = {
    val (noun, verb) = (for {
      noun <- 0 to 99
      verb <- 0 to 99 if run(input, noun, verb) == 19690720
    } yield (noun, verb)).head

    100 * noun + verb
  }

  def run(initialMemory: Memory, noun: Value, verb: Value): Value = {
    val memory = initialMemory.updated(1L, noun).updated(2L, verb)
    val (Return(_, state), _) = runProgram(noInput, ProgramState(memory))
    state.memory(0L)
  }

  private val input = parseMemory(Resources.string("day2.txt"))

  println(part1(input))
  println(part2(input))
}
