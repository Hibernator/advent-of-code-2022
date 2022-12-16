package ch.hibernator.adventofcode

import ch.hibernator.adventofcode.Day14.MoveResult
import ch.hibernator.adventofcode.Day14.MoveResult.{Abyss, NextCoordinates, Rest}
import ch.hibernator.adventofcode.Day14.State.{Air, Rock, Sand}

import scala.annotation.tailrec
import scala.collection.mutable

object Day14 extends SolutionBase {
  override def day: Int = 14

  case class Coordinates(col: Int, row: Int):
    override def toString: String = s"($col,$row)"

  enum State:
    case Air extends State
    case Rock extends State
    case Sand extends State

  case class Tile(coordinates: Coordinates, state: State):
    override def toString: String = {
      val stateSign = state match
        case Air  => "."
        case Rock => "#"
        case Sand => "o"
      s"$coordinates:$stateSign"
    }

  val rockPaths: Seq[Seq[Coordinates]] = input
    .map(_.split(" -> "))
    .map(_.map { rawCoordinates =>
      val rawNumbers = rawCoordinates.split(",")
      val (rawCol, rawRow) = (rawNumbers.head, rawNumbers.last)
      Coordinates(rawCol.toInt, rawRow.toInt)
    })

  println(rockPaths)

  val originalTerrain: mutable.Map[Coordinates, Tile] = mutable
    .Map()
    .withDefault(coordinates => Tile(coordinates, Air))

  rockPaths.foreach { path =>
    path.sliding(2).foreach { twoCoordinates =>
      val (start, end) = (twoCoordinates.head, twoCoordinates.last)
      val wholePath =
        if start.row == end.row then (start.col.min(end.col) to start.col.max(end.col)).map(Coordinates(_, start.row))
        else (start.row.min(end.row) to start.row.max(end.row)).map(Coordinates(start.col, _))
      wholePath.foreach(coordinates => originalTerrain.put(coordinates, Tile(coordinates = coordinates, state = Rock)))
    }
  }

  println(originalTerrain)

  val terrain: mutable.Map[Coordinates, Tile] = mutable.Map().withDefault(coordinates => Tile(coordinates, Air))
  terrain.addAll(originalTerrain.toSeq)

  val (minCol, maxCol) = {
    val columns = terrain.keys.map(_.col)
    (columns.min, columns.max)
  }
  var (minRow, maxRow) = (0, terrain.keys.map(_.row).max)

  def inAbyss(tile: Tile): Boolean = inAbyss(tile.coordinates)

//  def inAbyss(coordinates: Coordinates): Boolean = coordinates.col < minCol ||
//    coordinates.col > maxCol ||
//    coordinates.row > maxRow

  def inAbyss(coordinates: Coordinates): Boolean = coordinates.row > maxRow

  enum MoveResult:
    case Abyss extends MoveResult
    case Rest(coordinates: Coordinates) extends MoveResult
    case NextCoordinates(coordinates: Coordinates) extends MoveResult

  def oneSandMove(currentCoordinates: Coordinates, actualTerrain: mutable.Map[Coordinates, Tile]): MoveResult = {
    val tileDown = actualTerrain(currentCoordinates.copy(row = currentCoordinates.row + 1))
    lazy val tileDownLeft = actualTerrain(
      currentCoordinates.copy(col = currentCoordinates.col - 1, row = currentCoordinates.row + 1)
    )
    lazy val tileDownRight = actualTerrain(
      currentCoordinates.copy(col = currentCoordinates.col + 1, row = currentCoordinates.row + 1)
    )

    if tileDown.state == Air then if inAbyss(tileDown) then Abyss else NextCoordinates(tileDown.coordinates)
    else if tileDownLeft.state == Air then
      if inAbyss(tileDownLeft) then Abyss else NextCoordinates(tileDownLeft.coordinates)
    else if tileDownRight.state == Air then
      if inAbyss(tileDownRight) then Abyss else NextCoordinates(tileDownRight.coordinates)
    else Rest(currentCoordinates)
  }

  @tailrec
  def moveSandFrom(currentCoordinates: Coordinates, actualTerrain: mutable.Map[Coordinates, Tile]): MoveResult = {
    val moveResult = oneSandMove(currentCoordinates, actualTerrain)
    moveResult match
      case Abyss => Abyss
      case Rest(coordinates) =>
        actualTerrain.update(coordinates, Tile(coordinates = coordinates, state = Sand))
        Rest(coordinates)
      case NextCoordinates(coordinates) => moveSandFrom(coordinates, actualTerrain)
  }

  val startCoordinates: Coordinates = Coordinates(500, 0)

  @tailrec
  def moveAllSands(sandsMoved: Int): Int = {
    val moveResult = moveSandFrom(startCoordinates, terrain)
    if moveResult == Abyss then sandsMoved else moveAllSands(sandsMoved + 1)
  }

  val result = moveAllSands(0)
  println(result)

  val floorLevel = originalTerrain.keySet.map(_.row).max + 2
  maxRow = maxRow + 2

  val terrain2: mutable.Map[Coordinates, Tile] = mutable
    .Map()
    .withDefault(coordinates => Tile(coordinates, if coordinates.row == floorLevel then Rock else Air))
  terrain2.addAll(originalTerrain.toSeq)

  @tailrec
  def moveAllSands2(sandsMoved: Int): Int = {
    val moveResult = moveSandFrom(startCoordinates, terrain2)
    moveResult match
      case Rest(coordinates) =>
        if coordinates == startCoordinates then sandsMoved + 1 else moveAllSands2(sandsMoved + 1)
      case _ => sys.error("Shouldn't get to this state")
  }

  val result2 = moveAllSands2(0)
  println(result2)

}
