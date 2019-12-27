package aoc

import aoc.util.AsciiComputer.fromAscii
import aoc.util.IntcodeComputer._
import aoc.util.{AsciiComputer, Resources}

object Day21 extends App {

  sealed trait Register
  case object A extends Register
  case object B extends Register
  case object C extends Register
  case object D extends Register
  case object E extends Register
  case object F extends Register
  case object G extends Register
  case object H extends Register
  case object I extends Register

  sealed trait WritableRegister extends Register
  case object T                 extends WritableRegister
  case object J                 extends WritableRegister

  sealed trait SpringInstruction
  case class And(x: Register, y: WritableRegister) extends SpringInstruction
  case class Or(x: Register, y: WritableRegister)  extends SpringInstruction
  case class Not(x: Register, y: WritableRegister) extends SpringInstruction
  case object Walk                                 extends SpringInstruction
  case object Run                                  extends SpringInstruction

  def run(program: Memory, instructions: List[SpringInstruction]): String = {
    def toString(instruction: SpringInstruction): String = instruction match {
      case And(x, y) => s"AND $x $y"
      case Or(x, y)  => s"OR $x $y"
      case Not(x, y) => s"NOT $x $y"
      case Walk      => "WALK"
      case Run       => "RUN"
    }

    val input = instructions.map(toString).mkString("\n")

    val (_, output) = AsciiComputer.runProgram(ProgramState(program), Some(input))

    if (output.last > Byte.MaxValue) output.last.toString
    else fromAscii(output)
  }

  def part1(input: Memory): String = {
    // !(A && B && C) && D
    val instructions = List(
      Or(D, J),
      And(A, J),
      And(B, J),
      And(C, J),
      Not(J, J),
      And(D, J),
      Walk
    )
    run(input, instructions)
  }

  def part2(input: Memory): String = {
    // !(A && B && C) && D && (E || H)
    val instructions = List(
      Or(D, J),
      And(A, J),
      And(B, J),
      And(C, J),
      Not(J, J),
      And(D, J),
      Or(E, T),
      Or(H, T),
      And(T, J),
      Run
    )

    run(input, instructions)
  }

  private val input = parseMemory(Resources.string("day21.txt"))

  println(part1(input))
  println(part2(input))
}
