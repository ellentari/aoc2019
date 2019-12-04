package aoc

import scala.annotation.tailrec

object Day4 extends App {

  type Digit  = Int
  type Digits = List[Digit]

  def digits(num: Int): Digits =
    num.toString.map(ch => ch - '0').toList

  def allIncrease(digits: Digits): Boolean = {
    @tailrec
    def loop(previous: Digit, remaining: Digits): Boolean = remaining match {
      case Nil                              => true
      case head :: tail if previous <= head => loop(head, tail)
      case _                                => false
    }

    loop(0, digits)
  }

  def twoAdjacent(digits: Digits): Boolean =
    digits
      .zip(digits.tail)
      .exists(t => t._1 == t._2)

  def exactlyTwoAdjacent(digits: Digits): Boolean = {
    @tailrec
    def loop(previous: Digit, count: Int, remaining: Digits): Boolean = remaining match {
      case Nil if count == 2 => true
      case head :: tail =>
        if (head == previous) loop(head, count + 1, tail)
        else if (count == 2) true
        else loop(head, 1, tail)
      case _ => false
    }

    loop(-1, 1, digits)
  }

  def part1(from: Int, to: Int): Int =
    (from to to)
      .map(digits)
      .count(digits => twoAdjacent(digits) && allIncrease(digits))

  def part2(from: Int, to: Int): Int =
    (from to to)
      .map(digits)
      .count(digits => exactlyTwoAdjacent(digits) && allIncrease(digits))

  println(part1(178416, 676461))
  println(part2(178416, 676461))
}
