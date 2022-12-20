package ch.hibernator.adventofcode

import ch.hibernator.adventofcode.Day15.Thing.{Beacon, Sensor}

import scala.collection.mutable
import scala.util.control.Breaks.*
import scala.util.control.NonLocalReturns.*

object Day15 extends SolutionBase {
  override def day: Int = 15

  enum Thing:
    case Sensor extends Thing
    case Beacon extends Thing

  case class Coordinates(x: Int, y: Int)

  def distance(coordinates1: Coordinates, coordinates2: Coordinates): Int =
    (coordinates1.x - coordinates2.x).abs + (coordinates1.y - coordinates2.y).abs

  val lineRegex =
    "Sensor at x=([\\-]*[0-9]+), y=([\\-]*[0-9]+): closest beacon is at x=([\\-]*[0-9]+), y=([\\-]*[0-9]+)".r
  def parseLine(line: String): (Coordinates, Coordinates) = line match
    case lineRegex(sensorX, sensorY, beaconX, beaconY) =>
      (Coordinates(sensorX.toInt, sensorY.toInt), Coordinates(beaconX.toInt, beaconY.toInt))
    case _ => sys.error("Invalid line")

  val sensorToBeacon: Map[Coordinates, Coordinates] = input.foldLeft(Map[Coordinates, Coordinates]()) { (map, line) =>
    val (sensor, beacon) = parseLine(line)
    map.updated(sensor, beacon)
  }

  val terrain: Map[Coordinates, Thing] = sensorToBeacon.foldLeft(Map[Coordinates, Thing]()) {
    case (map, (sensor, beacon)) =>
      map.updated(sensor, Sensor).updated(beacon, Beacon)
  }

  val sensorToBeaconDistance: Map[Coordinates, Int] = sensorToBeacon.foldLeft(Map[Coordinates, Int]()) {
    case (map, (sensor, beacon)) =>
      map.updated(sensor, distance(sensor, beacon))
  }

  val minX = sensorToBeaconDistance.map((sensor, distance) => sensor.x - distance).min
  val maxX = sensorToBeaconDistance.map((sensor, distance) => sensor.x + distance).max
  val minY = sensorToBeaconDistance.map((sensor, distance) => sensor.y - distance).min
  val maxY = sensorToBeaconDistance.map((sensor, distance) => sensor.y + distance).max

  def numBeaconsInLine(line: Int): Int = terrain.filter((coordinates, _) => coordinates.y == line).count(_._1.y == line)

//  val lineY = 10
  val lineY = 2000000
  val beaconPossible: Array[Boolean] = Array.fill(maxX - minX + 1)(true)
  for (sensor <- sensorToBeaconDistance.keys) do
    val xsToCheck = beaconPossible.zipWithIndex.filter((possible, _) => possible).map(_._2 + minX)
    xsToCheck.foreach { x =>
      if distance(sensor, Coordinates(x, lineY)) <= sensorToBeaconDistance(sensor) then beaconPossible(x - minX) = false
    }

  val result = beaconPossible.count(_ == false) - numBeaconsInLine(lineY)

  println(result)

//  val (min, max) = (0, 20)
  val (min, max) = (0, 4000000)

  case class ManhattanSphere(center: Coordinates, radius: Int):
    def contains(point: Coordinates): Boolean =
      distance(center, point) <= radius && point.x >= min && point.x <= max && point.y >= min && point.y <= max

    def outsideCircle: Seq[Coordinates] =
      (for point <- ((center.x - radius - 1) to center.x).zip((center.y - radius - 1 to center.y).reverse)
      yield Coordinates(point._1, point._2)) ++
        (for point <- (center.x to (center.x + radius + 1)).zip(center.y - radius - 1 to center.y)
        yield Coordinates(point._1, point._2)) ++
        (for point <- (center.x to (center.x + radius + 1)).zip((center.y to center.y + radius + 1).reverse)
        yield Coordinates(point._1, point._2)) ++
        (for point <- (center.x - radius - 1 to center.x).zip(center.y to center.y + radius + 1)
        yield Coordinates(point._1, point._2))

  val allSpheres = sensorToBeacon.keys.map { point => ManhattanSphere(point, sensorToBeaconDistance(point)) }

  def findBeacon: Option[Coordinates] = {
    var actualResult: Option[Coordinates] = None
    breakable {
      for (sphere <- allSpheres) do
        val aroundCircle =
          sphere.outsideCircle.filter(point => point.x >= min && point.x <= max && point.y >= min && point.y <= max)
        val maybeResult = aroundCircle.find { point =>
          !allSpheres.filterNot(_ == sphere).exists(_.contains(point))
        }
        if maybeResult.isDefined then
          actualResult = maybeResult
          break()
    }

    actualResult
  }

  val missingBeacon = findBeacon
  val result2 = missingBeacon.map(beacon => beacon.x * 4000000L + beacon.y)
  println(result2)
}
