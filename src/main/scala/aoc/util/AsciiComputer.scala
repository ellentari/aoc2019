package aoc.util

import aoc.util.IntcodeComputer.{ProgramState, Return, Value, programInput}

object AsciiComputer {

  def runProgram(state: ProgramState, input: Option[String] = None): (ProgramState, String) = {
    def toAscii(s: String): Seq[Value] =
      s.toCharArray.map(_.toLong)

    def fromAscii(vs: List[Value]): String =
      new String(vs.map(_.toChar).toArray)

    val downstreamInput = input
      .map(i => programInput(toAscii(i + "\n"): _*))
      .getOrElse(() => None)

    val (Return(_, nextState), output) = IntcodeComputer.runProgram(downstreamInput, state)

    (nextState, fromAscii(output))
  }

}
