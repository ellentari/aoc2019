package aoc

import aoc.Data.{Direction, Point}
import aoc.util.IntcodeComputer.{GetInput, Memory, ProgramState, Return, Value}
import aoc.util.{IntcodeComputer, Resources}

import scala.annotation.tailrec

object Day13 extends App {

  type GameState = Map[Point, Tile]

  sealed trait Tile
  case object Empty              extends Tile
  case object Wall               extends Tile
  case object Block              extends Tile
  case object HorizontalPaddle   extends Tile
  case object Ball               extends Tile
  case class Score(value: Value) extends Tile

  sealed trait PaddleMove
  case object Left    extends PaddleMove
  case object Right   extends PaddleMove
  case object Neutral extends PaddleMove

  def part1(input: Memory): Int = {
    val (_, tiles) = run(ProgramState(input))

    countBlocks(tiles)
  }

  def part2(input: Memory): Value = {
    def findTile(state: GameState, tile: Tile): Option[Point] =
      state.toSeq
        .find(_._2 == tile)
        .map(_._1)

    def getScore(state: GameState): Value =
      state.getOrElse(Point(-1, 0), Empty) match {
        case Score(value) => value
        case _            => 0L
      }

    def paddleMove(ball: Point, paddle: Point): PaddleMove =
      if (paddle.x < ball.x) Right
      else if (paddle.x > ball.x) Left
      else Neutral

    @tailrec
    def loop(gameState: GameState, programState: ProgramState): GameState =
      if (countBlocks(gameState) == 0) gameState
      else {
        val ball   = findTile(gameState, Ball).get
        val paddle = findTile(gameState, HorizontalPaddle).get
        val input = paddleMove(ball, paddle) match {
          case Left    => -1
          case Right   => 1
          case Neutral => 0
        }

        val (state, output) = run(programState, IntcodeComputer.programInput(input))

        loop(gameState ++ output, state)
      }

    val initialTiles = run(ProgramState(input))._2
    val finalTiles   = loop(initialTiles, ProgramState(input.updated(0L, 2)))

    getScore(finalTiles)
  }

  private def run(programState: ProgramState, getInput: GetInput = () => None): (ProgramState, GameState) = {
    def tile(v: Value): Tile = v match {
      case 0 => Empty
      case 1 => Wall
      case 2 => Block
      case 3 => HorizontalPaddle
      case 4 => Ball
      case _ => Score(v)
    }

    val (Return(_, state), output) = IntcodeComputer.runProgram(getInput, programState)

    val tiles = output
      .grouped(3)
      .map {
        case x :: y :: tileId :: Nil => (Point(x.toInt, y.toInt), tile(tileId))
      }
      .toMap

    (state, tiles)
  }

  private def countBlocks(state: GameState): Int =
    state.count(_._2 == Block)

  val input = IntcodeComputer.parseMemory(Resources.string("day13.txt"))

  println(part1(input))
  println(part2(input))
}
