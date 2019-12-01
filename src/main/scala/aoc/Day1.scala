package aoc

import scala.annotation.tailrec

object Day1 extends App {

  def part1(input: List[String]): Int =
    input
      .map(_.toInt)
      .map(requiredFuel)
      .sum

  def part2(input: List[String]): Int =
    input
      .map(_.toInt)
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

  println(part1(Resources.lines("day1.txt")))
  println(part2(Resources.lines("day1.txt")))

}
