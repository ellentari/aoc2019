package aoc

import aoc.util.Resources

import scala.annotation.tailrec

object Day12 extends App {
  
  case class Point3D(x: Int, y: Int, z: Int)
  case class Velocity(dx: Int, dy: Int, dz: Int)
  case class Acceleration(dx: Int, dy: Int, dz: Int)

  case class Moon(position: Point3D, velocity: Velocity = Velocity(0, 0, 0))

  def parsePoint(s: String): Point3D = {
    val regexp = "<x=(-?\\d+), y=(-?\\d+), z=(-?\\d+)>".r

    s match {
      case regexp(x, y, z) => Point3D(x.toInt, y.toInt, z.toInt)
    }
  }

  def totalEnergy(moon: Moon): Int = {
    def potentialEnergy(p: Point3D): Int =
      List(p.x, p.y, p.z).map(_.abs).sum

    def kineticEnergy(v: Velocity): Int =
      List(v.dx, v.dy, v.dz).map(_.abs).sum

    potentialEnergy(moon.position) * kineticEnergy(moon.velocity)
  }

  def simulateStep(moon: Moon, other: List[Moon]): Moon = {

    def applyGravity(p1: Point3D, p2: Point3D): Acceleration = {
      def gravity(v1: Int, v2: Int): Int =
        if (v1 == v2) 0
        else if (v1 < v2) -1
        else 1

      Acceleration(dx = gravity(p1.x, p2.x), dy = gravity(p1.y, p2.y), dz = gravity(p1.z, p2.z))
    }

    def findAcceleration(moon: Moon, others: List[Moon]): Acceleration =
      others
        .map(_.position)
        .map(other => applyGravity(other, moon.position))
        .foldLeft(Acceleration(0, 0, 0))(
          (a1, a2) => Acceleration(dx = a1.dx + a2.dx, dy = a1.dy + a2.dy, dz = a1.dz + a2.dz)
        )

    def accelerate(v: Velocity, a: Acceleration): Velocity =
      Velocity(dx = v.dx + a.dx, dy = v.dy + a.dy, dz = v.dz + a.dz)

    def move(p: Point3D, v: Velocity): Point3D =
      Point3D(x = p.x + v.dx, y = p.y + v.dy, z = p.z + v.dz)

    val a  = findAcceleration(moon, other filter (_ != moon))
    val v1 = accelerate(moon.velocity, a)
    val p1 = move(moon.position, v1)

    Moon(p1, v1)
  }

  def simulate[R, S](initial: R)
                    (s: List[Moon] => S)
                    (until: (S, R) => Boolean)
                    (inc: (S, R) => R)
                    (moons: List[Moon]): (R, List[Moon]) = {

    @tailrec
    def loop(ms0: List[Moon], acc: R): (R, List[Moon]) = {
      val state = s(ms0)

      if (until(state, acc)) (acc, ms0)
      else {
        val ms1 = ms0 map (m0 => simulateStep(m0, ms0))
        loop(ms1, inc(state, acc))
      }
    }

    loop(moons, initial)
  }

  @tailrec
  def gcd(x: Long, y: Long): Long =
    if (y == 0) x
    else gcd(y, x % y)

  def lcm(x: Long, y: Long): Long = {
    x * y / gcd(x, y)
  }

  def part1(moons0: List[Moon], steps: Int): Int = {
    val moons1 = simulate(0)(identity)((_, c) => c == steps)((_, c) => c + 1)(moons0)._2

    moons1.map(totalEnergy).sum
  }

  def part2(moons0: List[Moon]): Long = {

    case class AxisState(v: Int, d: Int)
    case class MoonState(states: List[AxisState])

    def mkState(f: Point3D => Int, g: Velocity => Int): List[Moon] => MoonState =
      moons => MoonState(moons map (m => AxisState(f(m.position), g(m.velocity))))

    def oneAxisSimulate(f: Point3D => Int, g: Velocity => Int)(moons: List[Moon]): Int = {
      simulate((0: Int, Set.empty[MoonState]))(mkState(f, g))
      { case (state, (_, seen)) => seen.contains(state) }
      { case (state, (step, seen)) => (step + 1, seen + state) } (moons)._1._1
    }

    val x = oneAxisSimulate(_.x, _.dx)(moons0)
    val y = oneAxisSimulate(_.y, _.dy)(moons0)
    val z = oneAxisSimulate(_.z, _.dz)(moons0)

    lcm(lcm(x.toLong, y), z)
  }

  val input = Resources.lines("day12.txt")
    .map(parsePoint)
    .map(Moon(_))

  println(part1(input, 1000))
  println(part2(input))
}
