package aoc

import scala.io.Source

object Resources {

  def lines(path: String): List[String] =
    Source.fromResource(path).getLines().toList

}
