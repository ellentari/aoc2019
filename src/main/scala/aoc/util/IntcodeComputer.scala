package aoc.util

import java.lang.Math.pow

import scala.annotation.tailrec
import scala.collection.mutable

object IntcodeComputer {

  type Value   = Int
  type Address = Int

  type Memory = Map[Address, Value]

  type GetInput = () => Option[Value]
  type MkOutput = Value => Unit

  sealed trait Param
  case class Position(value: Address) extends Param
  case class Immediate(value: Value)  extends Param

  sealed trait Instruction
  case class Add(left: Param, right: Param, target: Address)   extends Instruction
  case class Times(left: Param, right: Param, target: Address) extends Instruction
  case class In(target: Address)                               extends Instruction
  case class Out(source: Param)                                extends Instruction
  case class JumpT(first: Param, second: Param)                extends Instruction
  case class JumpF(first: Param, second: Param)                extends Instruction
  case class Lt(first: Param, second: Param, target: Address)  extends Instruction
  case class Eq(first: Param, second: Param, target: Address)  extends Instruction
  case object Halt                                             extends Instruction

  sealed trait ReturnCode
  case object Exit  extends ReturnCode
  case object Block extends ReturnCode

  case class Return(code: ReturnCode, pointer: Address, memory: Memory)

  type Output = List[Value]

  def runProgram(memory: Memory, input: GetInput, start: Address = 0): (Return, Output) = {
    val output = mutable.ArrayBuffer[Value]()
    val after  = runLoop(start, memory, input, output.addOne)
    (after, output.toList)
  }

  private def runLoop(initial: Address, memory: Memory, input: GetInput, output: MkOutput): Return = {
    def eval(memory: Memory, param: Param): Value = param match {
      case Immediate(value)  => value
      case Position(address) => memory(address)
    }

    @tailrec
    def loop(memory: Memory, pointer: Address): Return =
      getInstruction(memory, pointer) match {
        case Halt => Return(Exit, pointer, memory)
        case Add(left, right, target) =>
          val result = eval(memory, left) + eval(memory, right)
          loop(memory.updated(target, result), pointer + 4)
        case Times(left, right, target) =>
          val result = eval(memory, left) * eval(memory, right)
          loop(memory.updated(target, result), pointer + 4)
        case In(target) => input() match {
          case None => Return(Block, pointer, memory)
          case Some(v) => loop(memory.updated(target, v), pointer + 2)
        }
        case Out(source) =>
          val result = eval(memory, source)
          output(result)
          loop(memory, pointer + 2)
        case JumpT(first, second) =>
          val value       = eval(memory, first)
          val nextPointer = if (value != 0) eval(memory, second) else pointer + 3
          loop(memory, nextPointer)
        case JumpF(first, second) =>
          val value       = eval(memory, first)
          val nextPointer = if (value == 0) eval(memory, second) else pointer + 3
          loop(memory, nextPointer)
        case Lt(first, second, target) =>
          val result = if (eval(memory, first) < eval(memory, second)) 1 else 0
          loop(memory.updated(target, result), pointer + 4)
        case Eq(first, second, target) =>
          val result = if (eval(memory, first) == eval(memory, second)) 1 else 0
          loop(memory.updated(target, result), pointer + 4)
      }

    loop(memory, initial)
  }

  private def getInstruction(memory: Memory, pointer: Address): Instruction = {
    def mem(position: Int): Value = memory(pointer + position)

    def param(position: Int): Param = {
      val paramValue = mem(position)

      (mem(0) / (100 * pow(10, position - 1).toInt)) % 10 match {
        case 0 => Position(paramValue)
        case 1 => Immediate(paramValue)
      }
    }

    val opcode = mem(0) % 100

    opcode match {
      case 99 => Halt
      case 1  => Add(param(1), param(2), mem(3))
      case 2  => Times(param(1), param(2), mem(3))
      case 3  => In(mem(1))
      case 4  => Out(param(1))
      case 5  => JumpT(param(1), param(2))
      case 6  => JumpF(param(1), param(2))
      case 7  => Lt(param(1), param(2), mem(3))
      case 8  => Eq(param(1), param(2), mem(3))
    }
  }
}
