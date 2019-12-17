package aoc

import java.lang.Math.pow

import aoc.Data._
import aoc.util.Resources

import scala.annotation.tailrec

object Day24 extends App {

  type Level = Int

  sealed trait Cell
  case object Empty           extends Cell
  case object Bug             extends Cell
  case object InnerLevelEntry extends Cell

  def nextCell(cell: Cell, adjacentBugs: Int) = cell match {
    case Bug =>
      if (adjacentBugs == 1) Bug
      else Empty
    case Empty =>
      if (adjacentBugs == 2 || adjacentBugs == 1) Bug
      else Empty
    case other => other
  }

  def findFirstRepetition(grid: Grid[Cell]): Grid[Cell] = {
    def adjacentCells(grid: Grid[Cell], point: Point): List[Cell] =
      grid
        .adjacent(point)
        .map(grid.getOrElse(_, Empty))

    def nextGrid(grid: Grid[Cell]): Grid[Cell] =
      grid.map {
        case (point, cell) =>
          val adjacentBugs = adjacentCells(grid, point).count(_ == Bug)

          (point, nextCell(cell, adjacentBugs))
      }

    @tailrec
    def loop(grid: Grid[Cell], seen: Set[Grid[Cell]]): Grid[Cell] =
      if (seen.contains(grid)) grid
      else loop(nextGrid(grid), seen + grid)

    loop(grid, Set())
  }

  def iterate(grid: Map[Level, Grid[Cell]], innerLevelEntry: Point, gridSize: Int, n: Int): Map[Level, Grid[Cell]] = {
    def emptyGrid: Grid[Cell] =
      (for {
        y <- 0 until gridSize
        x <- 0 until gridSize
        p = Point(x, y)
      } yield p -> Empty).toMap + (innerLevelEntry -> InnerLevelEntry)

    def adjacentPoints(grid: Map[Level, Grid[Cell]], level: Level, p: Point): Seq[(Point, Level)] = {
      val entryTop    = innerLevelEntry + (0, -1)
      val entryBottom = innerLevelEntry + (0, 1)
      val entryLeft   = innerLevelEntry + (-1, 0)
      val entryRight  = innerLevelEntry + (1, 0)

      def adjacentThisLevel: Seq[Point] =
        grid
          .getOrElse(level, emptyGrid)
          .adjacent(p)
          .filter(_ != innerLevelEntry)

      def adjacentAbove: Seq[Point] =
        Seq(
          if (p.x == 0) Some(entryLeft) else None,
          if (p.y == 0) Some(entryTop) else None,
          if (p.x == gridSize - 1) Some(entryRight) else None,
          if (p.y == gridSize - 1) Some(entryBottom) else None
        ).flatten

      def adjacentBelow: Seq[Point] =
        if (p == entryTop) (0 until gridSize).map(Point(_, 0))
        else if (p == entryLeft) (0 until gridSize).map(Point(0, _))
        else if (p == entryBottom) (0 until gridSize).map(Point(_, 4))
        else if (p == entryRight) (0 until gridSize).map(Point(4, _))
        else Nil

      val levelAbove = level - 1
      val levelBelow = level + 1

      adjacentThisLevel.map((_, level)) ++ adjacentAbove.map((_, levelAbove)) ++ adjacentBelow.map((_, levelBelow))
    }

    def adjacentCells(grid: Map[Level, Grid[Cell]], level: Level, point: Point): Seq[Cell] =
      adjacentPoints(grid, level, point).map {
        case (p, l) => grid.getOrElse(l, emptyGrid).getOrElse(p, Empty)
      }

    def nextGridLevel(grid: Map[Level, Grid[Cell]], level: Level): Grid[Cell] =
      grid.getOrElse(level, emptyGrid).map {
        case (point, cell) =>
          val adjacentBugs = adjacentCells(grid, level, point).count(_ == Bug)

          (point, nextCell(cell, adjacentBugs))
      }

    def nextGrid(grid: Map[Level, Grid[Cell]]): Map[Level, Grid[Cell]] = {
      def additionalLevel(level: Level): Map[Level, Grid[Cell]] =
        Some(nextGridLevel(grid, level))
          .filter(_.values.count(_ == Bug) > 0)
          .map(nextLvl => Map(level -> nextLvl))
          .getOrElse(Map())

      val nextGrid = grid.keys
        .map(level => (level, nextGridLevel(grid, level)))
        .toMap

      nextGrid ++ additionalLevel(grid.keys.min - 1) ++ additionalLevel(grid.keys.max + 1)
    }

    @tailrec
    def loop(grid: Map[Level, Grid[Cell]], n: Int): Map[Level, Grid[Cell]] =
      if (n == 0) grid
      else {
        val next = nextGrid(grid)

        loop(next, n - 1)
      }

    loop(grid, n)
  }

  def part1(input: List[String]): Long = {
    val grid   = parseGrid(input)
    val result = findFirstRepetition(grid)

    result
      .filter(_._2 == Bug)
      .keys
      .map(p => pow(2, p.y * input(p.y).length + p.x).toLong)
      .sum
  }

  def part2(input: List[String], n: Int): Long = {
    val gridSize        = input.length
    val innerLevelEntry = Point(gridSize / 2, gridSize / 2)

    val grid   = parseGrid(input) + (innerLevelEntry -> InnerLevelEntry)
    val result = iterate(Map(0 -> grid), innerLevelEntry, gridSize, n)

    result.values
      .flatMap(_.values)
      .count(_ == Bug)
  }

  def parseGrid(input: List[String]): Grid[Cell] =
    makeGridFromStrings(input) {
      case '.' => Empty
      case '#' => Bug
    }

  private val input = Resources.lines("day24.txt")

  println(part1(input))
  println(part2(input, 200))
}
