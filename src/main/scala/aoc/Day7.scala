package aoc

import aoc.util.IntcodeComputer._
import aoc.util.{IntcodeComputer, Resources}

import scala.annotation.tailrec

object Day7 extends App {

  type Phase = Int

  case class Amplifier(phase: Phase, pointer: Address, memory: Memory)

  sealed trait Feedback
  case object Continue extends Feedback
  case object Halt     extends Feedback

  def run(amplifiers: List[Amplifier]): Value = {

    def init(as: List[Amplifier]): List[Amplifier] =
      as.foldRight(List.empty[Amplifier]) {
        case (amp, acc) => run(amp, amp.phase)._2 :: acc
      }

    def iterate(firstInput: Value, as: List[Amplifier]): (Value, List[Amplifier], Feedback) =
      as.foldRight((firstInput, List.empty[Amplifier], Halt: Feedback)) {
        case (amp, (input, acc, _)) =>
          val (output, next, command) = run(amp, input)

          (output.get, next :: acc, command)
      }

    def run(amplifier: Amplifier, input: Value): (Option[Value], Amplifier, Feedback) = {
      val inputs = List(input).iterator

      val (_return, output) = IntcodeComputer.runProgram(
        amplifier.memory,
        () => if (inputs.hasNext) Some(inputs.next()) else None,
        amplifier.pointer
      )

      val command = _return.code match {
        case Exit => Halt
        case Block => Continue
      }

      (output.headOption, amplifier.copy(pointer = _return.pointer, memory = _return.memory), command)
    }

    @tailrec
    def loop(input: Value, amplifiers: List[Amplifier]): (Value, List[Amplifier]) = {
      val (output, next, command) = iterate(input, amplifiers)

      command match {
        case Halt     => (output, next)
        case Continue => loop(output, next)
      }
    }

    loop(0, init(amplifiers))._1
  }

  def findMax(input: String, possiblePhases: List[Phase]): Value = {
    val memory = Day5.parseMemory(input)

    possiblePhases
      .permutations
      .map(perm => perm.map(phase => Amplifier(phase, 0, memory)))
      .map(run)
      .max
  }

  def part1(input: String): Value = {
    findMax(input, List(0, 1, 2, 3, 4))
  }

  def part2(input: String): Value = {
    findMax(input, List(5, 6, 7, 8, 9))
  }

  private val input = Resources.string("day7.txt")

  println(part1(input))
  println(part2(input))
}
