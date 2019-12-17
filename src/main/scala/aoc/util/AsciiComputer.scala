package aoc.util

import aoc.util.IntcodeComputer.{Output, ProgramState, Return, Value, programInput}

object AsciiComputer {

  def runProgramToString(state: ProgramState, input: Option[String] = None): (ProgramState, String) = {
    def fromAscii(vs: List[Value]): String =
      new String(vs.map(_.toChar).toArray)

    val (nextState, output) = runProgram(state, input)

    (nextState, fromAscii(output))
  }

  def runProgram(state: ProgramState, input: Option[String] = None): (ProgramState, Output) = {
    def toAscii(s: String): Seq[Value] =
      s.toCharArray.map(_.toLong)

    val downstreamInput = input
      .map(i => programInput(toAscii(i + "\n"): _*))
      .getOrElse(() => None)

    val (Return(_, nextState), output) = IntcodeComputer.runProgram(downstreamInput, state)

    (nextState, output)
  }

}
