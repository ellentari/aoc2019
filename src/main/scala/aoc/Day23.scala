package aoc

import aoc.util.IntcodeComputer._
import aoc.util.{IntcodeComputer, Resources}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day23 extends App {

  type NetworkAddress = Int
  type Network        = Map[NetworkAddress, Machine]

  case class Machine(state: ProgramState, queue: Queue[Payload] = Queue.empty)
  case class Packet(recipient: NetworkAddress, x: Value, y: Value)
  case class NetworkStats(sent: Int = 0, received: Int = 0)

  sealed trait Command
  case class Receive(address: NetworkAddress)                                 extends Command
  case class Send(to: NetworkAddress, from: NetworkAddress, payload: Payload) extends Command
  case class CheckNetwork(stats: NetworkStats)                                extends Command

  sealed trait Payload
  case class Raw(value: Value)                 extends Payload
  case class PacketPayload(x: Value, y: Value) extends Payload

  sealed trait ExecResult[+S, +A]
  case class Continue[S](network: Network, stats: NetworkStats, state: S) extends ExecResult[S, Nothing]
  case class Done[A](result: A)                                           extends ExecResult[Nothing, A]

  val networkSize = 50
  val natAddress = 255

  def checkNetwork(network: Network, stats: NetworkStats): (List[Command], Network) = {
    val nat    = network(natAddress)
    val isIdle = stats.received + stats.sent == 0L

    nat.queue.dequeueOption match {
      case Some((p: PacketPayload, _)) if isIdle =>
        val updNetwork = network + (natAddress -> nat.copy(queue = Queue.empty))

        (List(Send(0, natAddress, p)), updNetwork)

      case _ => (List(), network)
    }
  }

  def runProgram(machine: Machine): (Machine, List[Packet], Int) = {
    val input = machine.queue.flatMap {
      case Raw(v)              => List(v)
      case PacketPayload(x, y) => List(x, y)
    }.toList

    val (nonEmptyInput, received) =
      if (input.isEmpty) (List(-1L), 0)
      else (input, input.size)

    val (Return(_, nextState), output) = IntcodeComputer.runProgram(programInput(nonEmptyInput: _*), machine.state)

    val sent = output
      .grouped(3)
      .collect {
        case address :: x :: y :: Nil => Packet(address.toInt, x, y)
      }
      .toList

    (Machine(state = nextState), sent, received)
  }

  def receive(network: Network, address: NetworkAddress): (List[Command], Network, NetworkStats) = {
    val machine = network(address)

    val (updMachine, sentPackets, received) = runProgram(machine)

    val updNetwork   = network + (address -> updMachine)
    val sendCommands = sentPackets.map(p => Send(p.recipient, address, PacketPayload(p.x, p.y)))

    (sendCommands, updNetwork, NetworkStats(received = received))
  }

  def processCommand(command: Command, network: Network): (List[Command], Network, NetworkStats) =
    command match {
      case CheckNetwork(stats) =>
        val (commands, updNetwork) = checkNetwork(network, stats)
        (commands, updNetwork, NetworkStats())

      case Receive(`natAddress`) =>
        (List(), network, NetworkStats())

      case Receive(address) =>
        receive(network, address)

      case Send(`natAddress`, _, payload) =>
        val nat = network(natAddress)

        val updNat     = nat.copy(queue = Queue(payload))
        val updNetwork = network + (natAddress -> updNat)

        (List(), updNetwork, NetworkStats(sent = 1))

      case Send(to, _, payload) =>
        val recipient = network(to)

        val updRecipient = recipient.copy(queue = recipient.queue :+ payload)
        val updNetwork   = network + (to -> updRecipient)

        (List(Receive(to)), updNetwork, NetworkStats(sent = 1))
    }

  def simulateNetwork[S, A](network: Network, initial: S, exit: (Command, S) => Either[S, A]): A = {

    def assignAddresses: ExecResult[S, A] = {
      val commands = network.keys.filter(_ != natAddress) map (address => Send(address, -1, Raw(address)))

      execCommands(Queue.from(commands), network, initial)
    }

    @tailrec
    def execCommands(queue: Queue[Command], network: Network, state: S, stats: NetworkStats = NetworkStats()): ExecResult[S, A] =
      queue.dequeueOption match {
        case None => Continue(network, stats, state)
        case Some((command, tail)) =>
          exit(command, state) match {
            case Right(b) => Done(b)
            case Left(updState) =>
              val (moreCommands, updNetwork, statsAfter) = processCommand(command, network)
              val updStats = NetworkStats(
                sent = stats.sent + statsAfter.sent,
                received = stats.received + statsAfter.received
              )

              execCommands(tail ++ moreCommands, updNetwork, updState, updStats)
          }
      }

    @tailrec
    def runLoop(network: Network, stats: NetworkStats, state: S): A = {
      val commands = CheckNetwork(stats) :: network.keys.filter(_ != natAddress).map(Receive).toList
      val result   = execCommands(Queue.from(commands), network, state)

      result match {
        case Continue(updNetwork, updStats, updState) => runLoop(updNetwork, updStats, updState)
        case Done(a)                                  => a
      }
    }

    assignAddresses match {
      case Done(a) => a
      case Continue(updNetwork, updStats, updState) => runLoop(updNetwork, updStats, updState)
    }
  }

  def initNetwork(program: Memory): Network = {
    val network = (0 until networkSize)
      .map(addr => (addr, Machine(ProgramState(program))))
      .toMap

    network + (natAddress -> Machine(ProgramState(program)))
  }

  def part1(program: Memory): Value = {
    def isDone(command: Command, state: Unit): Either[Unit, Value] = command match {
      case Send(`natAddress`, _, PacketPayload(_, y)) => Right(y)
      case _                                          => Left(())
    }

    val network = initNetwork(program)

    simulateNetwork(network, (), isDone)
  }

  def part2(program: Memory): Value = {
    def twoInARow(last: PacketPayload, previous: List[PacketPayload]): Boolean = previous match {
      case PacketPayload(_, y) :: _ if last.y == y => true
      case _                                       => false
    }

    def isDone(command: Command, state: List[PacketPayload]): Either[List[PacketPayload], Value] = command match {
      case Send(0, `natAddress`, p @ PacketPayload(_, y)) =>
        if (twoInARow(p, state)) Right(y)
        else Left(p :: state)
      case _ => Left(state)
    }

    val network = initNetwork(program)

    simulateNetwork(network, Nil, isDone)
  }

  private val input = parseMemory(Resources.string("day23.txt"))

  println(part1(input))
  println(part2(input))
}
