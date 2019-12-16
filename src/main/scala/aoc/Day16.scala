package aoc

import aoc.util.Resources

import scala.annotation.tailrec
import scala.collection.Iterator.{continually, fill}
import scala.collection.mutable

object Day16 extends App {

  def part1(input: List[Int], maxPhases: Int): String = {

    val basePattern = List(0, 1, 0, -1)

    @tailrec
    def loop(signal: List[Int], size: Int, phase: Int): List[Int] = {
      if (phase == maxPhases) signal
      else {
        val output = (1 to size).iterator
          .map(position =>
            continually(basePattern)
              .flatMap(base => base.iterator.flatMap(fill(position)(_)))
              .drop(1)
          )
          .map(pattern => signal.zip(pattern).map { case (el, p) => el * p }.sum)
          .map(_.abs)
          .map(_ % 10)
          .toList

        loop(output, size, phase + 1)
      }
    }

    val output = loop(input, input.size, 0)

    output.take(8).mkString("")
  }

  def part2(input: List[Int], maxPhases: Int): String = {

    @tailrec
    def loop(buffer: mutable.Buffer[Int], previous: IndexedSeq[Int], phase: Int): IndexedSeq[Int] = {
      if (phase == maxPhases) previous
      else {

        for {
          i <- Range(previous.size - 2, previous.size / 2, -1)
        } buffer(i) = (previous(i) + buffer(i + 1)) % 10

        loop(buffer, buffer.toIndexedSeq, phase + 1)
      }
    }

    val repeatedInput = (1 to 10000).flatMap(_ => input)
    val offset = input.take(7).mkString("").toInt

    val output = loop(repeatedInput.toBuffer, repeatedInput, 0)

    output.slice(offset, offset + 8).mkString("")
  }

  private def parse(input: String): List[Int] =
    input.toCharArray.map(_ - '0').toList

  private val input = parse(Resources.string("day16.txt"))

  println(part1(input, 100))
  println(part2(input, 100))

}
