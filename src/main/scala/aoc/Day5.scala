package aoc

import java.lang.Math.pow

import scala.annotation.tailrec
import scala.collection.mutable

object Day5 extends App {

  type Address = Int
  type Memory  = IndexedSeq[Address]
  type Input   = () => Int
  type Output  = Int => Unit

  sealed trait Param
  case class Position(value: Address) extends Param
  case class Immediate(value: Int)    extends Param

  sealed trait Instruction
  case class Add(left: Param, right: Param, target: Address)        extends Instruction
  case class Multiply(left: Param, right: Param, target: Address)   extends Instruction
  case class In(target: Address)                                    extends Instruction
  case class Out(source: Param)                                     extends Instruction
  case class JumpIfTrue(first: Param, second: Param)                extends Instruction
  case class JumpIfFalse(first: Param, second: Param)               extends Instruction
  case class LessThan(first: Param, second: Param, target: Address) extends Instruction
  case class Eq(first: Param, second: Param, target: Address)       extends Instruction
  case object Halt                                                  extends Instruction

  def eval(memory: Memory, param: Param) = param match {
    case Immediate(value)  => value
    case Position(address) => memory(address)
  }

  def getInstruction(memory: Memory, pointer: Address): Instruction = {
    def mem(position: Int) = memory(pointer + position)
    def param(position: Int): Param = {
      val paramValue = mem(position)

      (mem(0) / (100 * pow(10, position - 1).toInt)) % 10 match {
        case 0 => Position(paramValue)
        case 1 => Immediate(paramValue)
      }
    }

    val opcode = mem(0) % 100;

    opcode match {
      case 99 => Halt
      case 1  => Add(param(1), param(2), mem(3))
      case 2  => Multiply(param(1), param(2), mem( 3))
      case 3  => In(mem(1))
      case 4  => Out(param(1))
      case 5  => JumpIfTrue(param(1), param(2))
      case 6  => JumpIfFalse(param(1), param(2))
      case 7  => LessThan(param(1), param(2), mem(3))
      case 8  => Eq(param(1), param(2), mem(3))
    }
  }

  def run(memory: Memory, input: Input, output: Output): Unit = {
    @tailrec
    def loop(memory: Memory, pointer: Address): Memory =
      getInstruction(memory, pointer) match {
        case Halt => memory
        case Add(left, right, target) =>
          val result = eval(memory, left) + eval(memory, right)
          loop(memory.updated(target, result), pointer + 4)
        case Multiply(left, right, target) =>
          val result = eval(memory, left) * eval(memory, right)
          loop(memory.updated(target, result), pointer + 4)
        case In(target) =>
          val result = input()
          loop(memory.updated(target, result), pointer + 2)
        case Out(source) =>
          val result = eval(memory, source)
          output(result)
          loop(memory, pointer + 2)
        case JumpIfTrue(first, second) =>
          val value       = eval(memory, first)
          val nextPointer = if (value != 0) eval(memory, second) else pointer + 3
          loop(memory, nextPointer)
        case JumpIfFalse(first, second) =>
          val value       = eval(memory, first)
          val nextPointer = if (value == 0) eval(memory, second) else pointer + 3
          loop(memory, nextPointer)
        case LessThan(first, second, target) =>
          val result = if (eval(memory, first) < eval(memory, second)) 1 else 0
          loop(memory.updated(target, result), pointer + 4)
        case Eq(first, second, target) =>
          val result = if (eval(memory, first) == eval(memory, second)) 1 else 0
          loop(memory.updated(target, result), pointer + 4)
      }

    loop(memory, 0)
  }

  def runProgram(memory: Memory, input: Int): IndexedSeq[Int] = {
    val output = mutable.ArrayBuffer[Int]()
    run(memory, () => input, output.addOne)
    output.toIndexedSeq
  }

  def part1(memory: Memory): Int = {
    val output = runProgram(memory, 1)
    output.last
  }

  def part2(memory: Memory): Int = {
    val output = runProgram(memory, 5)
    output.head
  }

  private val input = Resources.string("day5.txt")
    .split(",")
    .map(_.toInt)
    .toIndexedSeq

  println(part1(input))
  println(part2(input))
}
