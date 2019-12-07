package aoc

import aoc.util.IntcodeComputer.{Memory, Output, Value}
import aoc.util.{IntcodeComputer, Resources}

object Day5 extends App {

  def runProgram(memory: Memory, input: Value): Output =
    IntcodeComputer.runProgram(memory, () => Some(input))._2

  def part1(memory: Memory): Int = {
    runProgram(memory, 1).last
  }

  def part2(memory: Memory): Int = {
    runProgram(memory, 5).head
  }

  def parseMemory(s: String): Memory =
    s.split(",")
      .map(_.toInt)
      .zipWithIndex
      .map(_.swap)
      .toMap

  private val input = parseMemory(Resources.string("day5.txt"))

  println(part1(input))
  println(part2(input))
}
