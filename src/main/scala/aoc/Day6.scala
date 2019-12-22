package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day6 extends App {

  type Planet   = String
  type Distance = Int
  type Orbits   = Int

  case class Edge(orbit: Planet, inOrbit: Planet)
  case class Graph(inOrbit: Map[Planet, List[Planet]], orbit: Map[Planet, Planet])

  case class Move(planet: Planet, distance: Distance)

  def part1(edges: List[Edge]): Orbits = {
    @tailrec
    def loop(planets: Graph, stack: List[Edge], acc: Map[Planet, Orbits]): Map[Planet, Orbits] = stack match {
      case Edge(orbit, inOrbit) :: tail if !acc.contains(inOrbit) =>
        val next = planets.inOrbit(inOrbit).map(Edge(inOrbit, _))
        loop(planets, next ++ tail, acc + (inOrbit -> (acc(orbit) + 1)))
      case _ :: tail => loop(planets, tail, acc)
      case Nil       => acc
    }

    val planets = makeGraph(edges)

    val source = planets.inOrbit.keys
      .find(!planets.orbit.contains(_))
      .get

    loop(planets, planets.inOrbit(source).map(Edge(source, _)), Map().withDefaultValue(0)).values.sum
  }

  def part2(edges: List[Edge]): Distance = {
    val graph = makeGraph(edges)

    def next(cur: Move): List[Move] = {
      val adjacent = graph.orbit.get(cur.planet).toList ++ graph.inOrbit(cur.planet)

      adjacent map (Move(_, cur.distance + 1))
    }

    val start       = graph.orbit("YOU")
    val destination = graph.orbit("SAN")

    Search.runBFS(List(Move(start, 0)))(next)(_.planet == destination)(_.planet).toOption.get.distance
  }

  def parseEdge(edge: String): Edge =
    edge.split("\\)") match {
      case Array(orbit, inOrbit) => Edge(orbit, inOrbit)
    }

  def makeGraph(edges: List[Edge]): Graph = {
    val orbits = edges.map(e => (e.inOrbit, e.orbit)).toMap

    val inOrbit = edges
      .groupBy(_.orbit)
      .view
      .mapValues(_.map(_.inOrbit))
      .toMap
      .withDefaultValue(List())

    Graph(inOrbit, orbits)
  }

  val input = Resources.lines("day6.txt") map parseEdge

  println(part1(input))
  println(part2(input))
}
