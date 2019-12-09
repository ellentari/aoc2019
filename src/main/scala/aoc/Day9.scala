package aoc

import aoc.util.IntcodeComputer.{Memory, Value}
import aoc.util.{IntcodeComputer, Resources}

object Day9 extends App {

  def part1(memory: Memory): Value =
    run(memory, 1)

  def part2(memory: Memory): Value =
    run(memory, 2)

  private def run(memory: Memory, input: Int): Value =
    IntcodeComputer.runProgram(memory, () => Some(input))._2.head

  private val input = IntcodeComputer.parseMemory(Resources.string("day9.txt"))

  println(part1(input))
  println(part2(input))

}
