package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day1 extends App {

  def parseInput(lines: List[String]): List[Int] =
    lines.map(_.toInt)

  def part1(input: List[Int]): Int =
    input
      .map(requiredFuel)
      .sum

  def part2(input: List[Int]): Int =
    input
      .map(requiredFuelRec)
      .sum

  def requiredFuel(mass: Int): Int =
    mass / 3 - 2

  def requiredFuelRec(mass: Int): Int = {
    @tailrec
    def loop(acc: Int, m: Int): Int = {
      val fuel = requiredFuel(m)
      if (fuel <= 0) acc
      else loop(acc + fuel, fuel)
    }

    loop(0, mass)
  }

  private val input = parseInput(Resources.lines("day1.txt"))

  println(part1(input))
  println(part2(input))

}
