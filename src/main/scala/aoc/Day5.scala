package aoc

import aoc.util.IntcodeComputer.{Memory, Output, ProgramState, Value, programInput}
import aoc.util.{IntcodeComputer, Resources}

object Day5 extends App {

  def runProgram(memory: Memory, input: Value): Output =
    IntcodeComputer.runProgram(programInput(input), ProgramState(memory))._2

  def part1(memory: Memory): Value = {
    runProgram(memory, 1L).last
  }

  def part2(memory: Memory): Value = {
    runProgram(memory, 5L).head
  }

  private val input = IntcodeComputer.parseMemory(Resources.string("day5.txt"))

  println(part1(input))
  println(part2(input))
}
