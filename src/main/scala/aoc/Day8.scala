package aoc

import aoc.util.Resources

object Day8 extends App {

  sealed trait Color
  case object Black       extends Color
  case object White       extends Color
  case object Transparent extends Color

  type Layer = IndexedSeq[IndexedSeq[Color]]

  val width = 25
  val height = 6

  def parse(input: String, w: Int, h: Int): List[List[String]] = {
    input
      .grouped(w)
      .grouped(h)
      .map(_.toList)
      .toList
  }

  def toColor(ch: Char): Color = ch match {
    case '2' => Transparent
    case '0' => Black
    case '1' => White
  }

  def part1(input: String): Int = {
    val minByZeroes = parse(input, width, height)
      .map(_.flatten)
      .minBy(_.count(_ == '0'))

    minByZeroes.count(_ == '1') * minByZeroes.count(_ == '2')
  }

  def part2(input: String): List[String] = {

    def parseLayers(input: String) = {
      parse(input, width, height)
        .map(_.map(_.map(toColor _)).toIndexedSeq)
    }

    def colorAt(layers: List[Layer], h: Int, w: Int): Color = {
      def merge(top: Color, bottom: Color) =
        if (top == Transparent) bottom
        else top

      layers.foldLeft(Transparent: Color)((acc, layer) => merge(acc, layer(h)(w)))
    }

    def render(layers: List[Layer]): Layer = {
      val matrix = Array.ofDim[Color](height, width)

      for {
        h <- 0 until height
        w <- 0 until width
      } matrix(h)(w) = colorAt(layers, h, w)

      matrix.map(_.toIndexedSeq).toIndexedSeq
    }

    val image = parseLayers(input)

    render(image)
      .iterator
      .map(_.map(c => if (c == Black) ' ' else '*').mkString)
      .toList
  }

  val input = Resources.string("day8.txt")

  println(part1(input))
  println(part2(input).mkString("\n"))
}
