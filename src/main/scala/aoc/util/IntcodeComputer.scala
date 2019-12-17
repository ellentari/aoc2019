package aoc.util

import java.lang.Math.pow

import scala.annotation.tailrec
import scala.collection.mutable

object IntcodeComputer {

  type Value   = Long
  type Address = Long

  type Memory = Map[Address, Value]

  type GetInput = () => Option[Value]
  type MkOutput = Value => Unit

  sealed trait Param
  case class Position(value: Address) extends Param
  case class Immediate(value: Value)  extends Param
  case class Relative(value: Address) extends Param

  sealed trait Instruction
  case class Add(left: Param, right: Param, target: Param)      extends Instruction
  case class Times(left: Param, right: Param, target: Param)    extends Instruction
  case class In(target: Param)                                  extends Instruction
  case class Out(source: Param)                                 extends Instruction
  case class JumpT(first: Param, second: Param)                 extends Instruction
  case class JumpF(first: Param, second: Param)                 extends Instruction
  case class Lt(first: Param, second: Param, target: Param)     extends Instruction
  case class Eq(first: Param, second: Param, target: Param)     extends Instruction
  case class RelBaseOffset(value: Param)                        extends Instruction
  case object Halt                                              extends Instruction

  sealed trait ReturnCode
  case object Exit  extends ReturnCode
  case object Block extends ReturnCode

  case class ProgramState(memory: Memory, pointer: Address = 0L, relativeBase: Address = 0L)
  case class Return(code: ReturnCode, state: ProgramState)

  type Output = List[Value]

  def parseMemory(memory: String): Memory =
    memory.split(",")
      .map(_.toLong)
      .zipWithIndex
      .map(kv => (kv._2.toLong, kv._1))
      .toMap
      .withDefault(k => if (k >= 0) 0L else throw new NoSuchElementException(k.toString))

  def runProgram(input: GetInput, state: ProgramState): (Return, Output) = {
    val output = mutable.ArrayBuffer[Value]()
    val after  = runLoop(state, input, output.addOne)
    (after, output.toList)
  }

  private def runLoop(state: ProgramState, input: GetInput, output: MkOutput): Return = {

    def eval(state: ProgramState, param: Param): Value = param match {
      case Immediate(value)  => value
      case Position(address) => state.memory(address)
      case Relative(address) => state.memory(state.relativeBase + address)
    }

    def evalWritePosition(state: ProgramState, param: Param): Address = param match {
      case Position(address) => address
      case Relative(address) => state.relativeBase + address
    }

    def write(value: Value, state: ProgramState, target: Param): Memory = {
      val address = evalWritePosition(state, target)
      state.memory.updated(address, value)
    }

    @tailrec
    def loop(state: ProgramState): Return = {
      val pointer      = state.pointer
      val memory       = state.memory

      getInstruction(memory, pointer) match {
        case Halt => Return(Exit, state)
        case Add(left, right, target) =>
          val result = eval(state, left) + eval(state, right)
          loop(state.copy(memory = write(result, state, target), pointer = pointer + 4))
        case Times(left, right, target) =>
          val result = eval(state, left) * eval(state, right)
          loop(state.copy(memory = write(result, state, target), pointer = pointer + 4))
        case In(target) => input() match {
          case None => Return(Block, state)
          case Some(v) => loop(state.copy(memory = write(v, state, target), pointer = pointer + 2))
        }
        case Out(source) =>
          val result = eval(state, source)
          output(result)
          loop(state.copy(pointer = pointer + 2))
        case JumpT(first, second) =>
          val value       = eval(state, first)
          val nextPointer = if (value != 0) eval(state, second) else pointer + 3
          loop(state.copy(pointer = nextPointer))
        case JumpF(first, second) =>
          val value       = eval(state, first)
          val nextPointer = if (value == 0) eval(state, second) else pointer + 3
          loop(state.copy(pointer = nextPointer))
        case Lt(first, second, target) =>
          val result = if (eval(state, first) < eval(state, second)) 1 else 0
          loop(state.copy(memory = write(result, state, target), pointer = pointer + 4))
        case Eq(first, second, target) =>
          val result = if (eval(state, first) == eval(state, second)) 1 else 0
          loop(state.copy(memory = write(result, state, target), pointer = pointer + 4))
        case RelBaseOffset(param) =>
          val value = eval(state, param)
          val nextBase = state.relativeBase + value
          loop(state.copy(relativeBase = nextBase, pointer = pointer + 2))
      }
    }

    loop(state)
  }

  private def getInstruction(memory: Memory, pointer: Address): Instruction = {
    def mem(position: Int): Value = memory(pointer + position)

    def param(position: Int): Param = {
      val value = mem(position)

      (mem(0) / (100 * pow(10, position - 1).toInt)) % 10 match {
        case 0 => Position(value)
        case 1 => Immediate(value)
        case 2 => Relative(value)
      }
    }

    val opcode = mem(0) % 100

    opcode match {
      case 99 => Halt
      case 1  => Add(param(1), param(2), param(3))
      case 2  => Times(param(1), param(2), param(3))
      case 3  => In(param(1))
      case 4  => Out(param(1))
      case 5  => JumpT(param(1), param(2))
      case 6  => JumpF(param(1), param(2))
      case 7  => Lt(param(1), param(2), param(3))
      case 8  => Eq(param(1), param(2), param(3))
      case 9  => RelBaseOffset(param(1))
    }
  }

  def programInput(values: Value*): GetInput = {
    val iterator = values.iterator
    () => if (iterator.hasNext) Some(iterator.next()) else None
  }

  def noInput: GetInput =
    () => None

}
