package aoc

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Search {

  type BFSResult[A, B] = Either[Set[A], B]

  def runBFS[A, B, C](initial: List[A])
                     (next: A => List[A])
                     (isDone: A => Boolean)
                     (toSeen: A => B): BFSResult[B, A] = {

    @tailrec
    def loop(queue: Queue[A], seen: Set[B]): Either[Set[B], A] =
      queue.dequeueOption match {
        case None => Left(seen)
        case Some((top, tail)) =>
          if (isDone(top)) Right(top)
          else if (seen.contains(toSeen(top))) loop(tail, seen)
          else {
            val more = next(top).filterNot(m => seen.contains(toSeen(m)))

            loop(tail ++ more, seen + toSeen(top))
          }
      }

    loop(Queue.from(initial), Set())
  }

}
