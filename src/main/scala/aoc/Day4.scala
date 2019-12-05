package aoc

object Day4 extends App {

  type Digit  = Int
  type Digits = List[Digit]

  def allIncrease(digits: Digits): Boolean =
    digits.zip(digits.tail).forall {
      case (a, b) => a <= b
    }

  def twoAdjacent(digits: Digits): Boolean =
    digits.zip(digits.tail).exists {
      case (a, b) => a == b
    }

  def exactlyTwoAdjacent(digits: Digits): Boolean =
    (10 :: (digits :+ 10)).tails
      .filter(_.size >= 4)
      .map(_.take(4))
      .exists {
        case a :: b :: c :: d :: Nil => a != b && b == c && c != d
      }

  def countPasswords(isGood: Digits => Boolean)(from: Int, to: Int) = {
    def digits(num: Int) = num.toString.map(ch => ch - '0').toList

    (from to to)
      .map(digits)
      .count(isGood)
  }

  def part1: (Int, Int) => Int = {
    def isGood(digits: Digits) = allIncrease(digits) && twoAdjacent(digits)

    countPasswords(isGood)
  }

  def part2: (Int, Int) => Int = {
    def isGood(digits: Digits) = allIncrease(digits) && exactlyTwoAdjacent(digits)

    countPasswords(isGood)
  }

  println(part1(178416, 676461))
  println(part2(178416, 676461))
}
