package aoc

import aoc.util.IntcodeComputer._
import aoc.util.{IntcodeComputer, Resources}

import scala.annotation.tailrec

object Day7 extends App {

  type Phase = Int

  case class Amplifier(phase: Phase, programState: ProgramState)

  sealed trait Feedback
  case object Continue extends Feedback
  case object Halt     extends Feedback

  case class ProgramOutput(value: Option[Value], feedback: Feedback)

  def runProgram(programState: ProgramState, input: Value): (ProgramOutput, ProgramState) = {
    val (Return(code, nextState), output) = IntcodeComputer.runProgram(programInput(input), programState)

    val feedback = code match {
      case Exit  => Halt
      case Block => Continue
    }

    (ProgramOutput(output.headOption, feedback), nextState)
  }

  def run(amplifiers: List[Amplifier]): Value = {

    def init(amplifiers: List[Amplifier]): List[Amplifier] =
      amplifiers
        .map(a => {
          val nextState = runProgram(a.programState, a.phase)._2
          a.copy(programState = nextState)
        })

    def runAmplifiers(input: Value, as: List[Amplifier]): (ProgramOutput, List[Amplifier]) =
      as.foldRight((ProgramOutput(Some(input), Halt), List.empty[Amplifier])) {
        case (amp, (last, acc)) =>
          val (output, nextState) = runProgram(amp.programState, last.value.get)

          (output, amp.copy(programState = nextState) :: acc)
      }

    @tailrec
    def loop(input: Value, amplifiers: List[Amplifier]): Value = {
      val (result, next) = runAmplifiers(input, amplifiers)

      result.feedback match {
        case Halt     => result.value.get
        case Continue => loop(result.value.get, next)
      }
    }

    loop(0L, init(amplifiers))
  }

  def findMax(program: Memory, possiblePhases: List[Phase]): Value =
    possiblePhases
      .permutations
      .map(_.map(Amplifier(_, ProgramState(program))))
      .map(run)
      .max

  def part1(input: Memory): Value = {
    findMax(input, List(0, 1, 2, 3, 4))
  }

  def part2(input: Memory): Value = {
    findMax(input, List(5, 6, 7, 8, 9))
  }

  private val input = parseMemory(Resources.string("day7.txt"))

  println(part1(input))
  println(part2(input))
}
