package ch.hibernator.adventofcode

import scala.collection.mutable

object Day12 extends SolutionBase {
  override def day: Int = 12

  val realInput = input

  def height(pos: Vertex): Char = realInput(pos.row)(pos.col)

  val startRow = realInput.indexWhere(_.contains("S"))
  val source = Vertex(startRow, realInput(startRow).indexOf('S'))
  val endRow = realInput.indexWhere(_.contains("E"))
  val target = Vertex(endRow, realInput(endRow).indexOf('E'))
  val maxRow = realInput.size - 1
  val maxCol = realInput.head.length - 1

  case class Vertex(row: Int, col: Int):

    lazy val currentHeight: Char = height(this)

    override def toString: String = s"($row, $col)"

    def neighbors: List[Vertex] =
//      println(s"neighbors of $row, $col")
      val neighbors =
        List(Vertex(row, col - 1), Vertex(row, col + 1), Vertex(row - 1, col), Vertex(row + 1, col))
          .filter(pos => pos.row >= 0 && pos.row <= maxRow && pos.col >= 0 && pos.col <= maxCol)
//      println(neighbors)
      neighbors

    def validMoves: List[Vertex] =
      val moves = neighbors.filter { pos =>
        val newHeight = height(pos)
        newHeight == currentHeight ||
        newHeight == currentHeight + 1 ||
        (newHeight < currentHeight && newHeight != 'E' && newHeight >= 'm') ||
        (currentHeight == 'S' && newHeight == 'a') ||
//        newHeight == 'S' ||
        ((currentHeight == 'y' || currentHeight == 'z') && newHeight == 'E')
      }
//      println(s"validMoves from $row, $col")
//      println(moves)
      moves

    def isGoal: Boolean = currentHeight == 'E'

  class ExtendedVertex(val vertex: Vertex, var distance: Int = 10_000, var previous: Option[Vertex] = None):
    override def toString: String = s"${vertex.toString}: $distance: ${previous.map(_.toString).getOrElse("None")}"

  val allVertices: Set[Vertex] = (for
    row <- 0 to maxRow
    col <- 0 to maxCol
  yield Vertex(row, col)).to(Set)

  def shortestPathSize(actualSource: Vertex): Int = {
    val verticesToProcess: mutable.Set[Vertex] = allVertices.to(mutable.Set)

    val vertexToExtendedVertex: mutable.Map[Vertex, ExtendedVertex] = mutable.Map()
    verticesToProcess.foreach(vertex => vertexToExtendedVertex.addOne(vertex, ExtendedVertex(vertex)))

    vertexToExtendedVertex(actualSource).distance = 0

    val allExtendedVerticesToProcess: mutable.Set[ExtendedVertex] = vertexToExtendedVertex.values.to(mutable.Set)

    while allExtendedVerticesToProcess.nonEmpty do
      val currentVertex = allExtendedVerticesToProcess.minBy(_.distance)
      allExtendedVerticesToProcess.subtractOne(currentVertex)
      verticesToProcess.subtractOne(currentVertex.vertex)
      val newNeighbors = currentVertex.vertex.validMoves.filter(verticesToProcess.contains)
      newNeighbors.foreach { neighbor =>
        val alternativeDistance = currentVertex.distance + 1
        val extendedNeighbor = vertexToExtendedVertex(neighbor)
        if alternativeDistance <= extendedNeighbor.distance then
          extendedNeighbor.distance = alternativeDistance
          extendedNeighbor.previous = Some(currentVertex.vertex)
      }

    vertexToExtendedVertex(target).distance
  }

  val result = shortestPathSize(source)
  println(result)

  val possibleStarts = allVertices.filter(_.currentHeight == 'a') + source
  val allShortestPaths = possibleStarts.map(shortestPathSize)
  val result2 = allShortestPaths.min
  println(result2)

}
