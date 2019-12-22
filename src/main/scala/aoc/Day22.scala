package aoc

import aoc.util.Resources

object Day22 extends App {

  sealed trait Technique
  case class Cut(n: Long)              extends Technique
  case class DealWithIncrement(n: Int) extends Technique
  case object DealNewStack             extends Technique

  private def parse(s: String): Technique = {
    val cut     = "cut (-?\\d+)".r
    val dealInc = "deal with increment (\\d+)".r

    s match {
      case "deal into new stack" => DealNewStack
      case cut(n)                => Cut(n.toInt)
      case dealInc(n)            => DealWithIncrement(n.toInt)
    }
  }

  def simulate(deckSize: Int, techniques: List[Technique]): IndexedSeq[Int] = {
    def dealNewStack(deck: IndexedSeq[Int]): IndexedSeq[Int] =
      deck.reverse

    def cut(deck: IndexedSeq[Int], n: Int): IndexedSeq[Int] = {
      val partition = if (n > 0) n else deck.length + n

      deck.drop(partition) ++ deck.take(partition)
    }

    def dealWithIncrement(deck: IndexedSeq[Int], n: Int): IndexedSeq[Int] = {
      val buffer = deck.toBuffer

      deck.zipWithIndex.foreach {
        case (card, index) => buffer((index * n) % deck.size) = card
      }

      buffer.toIndexedSeq
    }

    def shuffle(deck: IndexedSeq[Int], technique: Technique): IndexedSeq[Int] = technique match {
      case DealNewStack         => dealNewStack(deck)
      case Cut(n)               => cut(deck, n.toInt)
      case DealWithIncrement(n) => dealWithIncrement(deck, n)
    }

    techniques.foldLeft(0 until deckSize: IndexedSeq[Int])(shuffle)
  }

  def part1(deckSize: Int, card: Int, techniques: List[Technique]): Int =
    simulate(deckSize, techniques).indexOf(card)

  def part2(deckSize: Long, repetitions: Long, position: Long, techniques: List[Technique]): Long = {

    def modPov(value: BigInt, exp: Long): BigInt = value.modPow(exp, deckSize)
    def mod(value: BigInt): BigInt = value % deckSize
    def inv(value: BigInt): BigInt = value.modPow(deckSize - 2, deckSize)

    def shuffle(offset: BigInt, increment: BigInt, technique: Technique): (BigInt, BigInt) =
      technique match {
        case DealNewStack         => (mod(offset + -increment), mod(-increment))
        case Cut(n)               => (mod(offset + increment * n), increment)
        case DealWithIncrement(n) => (offset, mod(increment * inv(n)))
      }

    def repeat(offsetDiff: BigInt, incrementMul: BigInt, times: Long): (BigInt, BigInt) = {
      val increment = modPov(incrementMul, times)
      val offset    = mod(offsetDiff * (1 - increment) * inv(mod(1 - incrementMul)))

      (offset, increment)
    }

    def get(offset: BigInt, increment: BigInt, n: Long): Long =
      mod(offset + n * increment).toLong

    val (offsetDiff, incrementMul) = techniques.foldLeft((BigInt(0), BigInt(1))) {
      case ((offset, increment), technique) => shuffle(offset, increment, technique)
    }

    val (offset, increment) = repeat(offsetDiff, incrementMul, repetitions)

    get(offset, increment, position)
  }

  private val input = Resources.lines("day22.txt") map parse

  println(part1(deckSize = 10007, card = 2019, techniques = input))
  println(part2(deckSize = 119315717514047L, repetitions = 101741582076661L, position = 2020, techniques = input))
}
