package aoc.util

import scala.io.Source

object Resources {

  def lines(path: String): List[String] =
    Source.fromResource(path).getLines().toList

  def string(path: String): String =
    lines(path).mkString

}
