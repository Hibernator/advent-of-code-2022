package ch.hibernator.adventofcode

import ch.hibernator.adventofcode.Day9.Direction
import ch.hibernator.adventofcode.Day9.Direction.{East, North, NorthEast, NorthWest, South, SouthEast, SouthWest, West}

object Day9 extends SolutionBase {
  override def day: Int = 9

  enum Direction(sign: Char):
    case North extends Direction('U')
    case South extends Direction('D')
    case West extends Direction('L')
    case East extends Direction('R')
    case NorthEast extends Direction('A')
    case SouthEast extends Direction('B')
    case SouthWest extends Direction('C')
    case NorthWest extends Direction('D')

  object Direction:
    def forSign(sign: Char): Direction =
      sign match
        case 'U' => North
        case 'D' => South
        case 'L' => West
        case 'R' => East

  case class Coordinates(x: Int, y: Int):

    override def toString: String = s"($x,$y)"

    def move(direction: Direction): Coordinates =
      direction match
        case North     => Coordinates(x, y + 1)
        case South     => Coordinates(x, y - 1)
        case West      => Coordinates(x - 1, y)
        case East      => Coordinates(x + 1, y)
        case NorthEast => Coordinates(x + 1, y + 1)
        case SouthEast => Coordinates(x + 1, y - 1)
        case SouthWest => Coordinates(x - 1, y - 1)
        case NorthWest => Coordinates(x - 1, y + 1)

    def isAdjacent(other: Coordinates): Boolean =
      (other.x - x).abs <= 1 && (other.y - y).abs <= 1

    def directionOfOther(other: Coordinates): Direction =
      if other.x == x && other.y > y then North
      else if other.x > x && other.y > y then NorthEast
      else if other.x > x && other.y == y then East
      else if other.x > x && other.y < y then SouthEast
      else if other.x == x && other.y < y then South
      else if other.x < x && other.y < y then SouthWest
      else if other.x < x && other.y == y then West
      else NorthWest

  case class State(head: Coordinates, tail: Coordinates, visited: Set[Coordinates])

  val moves = input
    .map { line =>
      val direction = Direction.forSign(line.head)
      val distance = line.drop(2).toInt
      (direction, distance)
    }
    .flatMap { (direction, distance) =>
      Seq.fill(distance)(direction)
    }

  val finalState = moves.foldLeft(State(Coordinates(0, 0), Coordinates(0, 0), Set(Coordinates(0, 0)))) {
    (state, move) =>
      val newHead = state.head.move(move)
      if state.tail.isAdjacent(newHead) then State(newHead, state.tail, state.visited)
      else
        val tailMoveDirection = state.tail.directionOfOther(newHead)
        val newTail = state.tail.move(tailMoveDirection)
        State(newHead, newTail, state.visited + newTail)
  }

  val result = finalState.visited.size
  println(result)

  case class LongRopeState(head: Coordinates, tail: Seq[Coordinates], visited: Set[Coordinates])

  val finalLongRopeState = moves.foldLeft(
    LongRopeState(Coordinates(0, 0), Seq.fill(9)(Coordinates(0, 0)), Set(Coordinates(0, 0)))
  ) { (currentState, move) =>
    val newHead = currentState.head.move(move)
    val newStateStart = LongRopeState(newHead, Seq(), currentState.visited)
    val newStateFinished = (newHead +: currentState.tail.init).foldLeft(newStateStart) { (newState, leadingKnot) =>
      val followingKnot = currentState.tail(newState.tail.size)
      val actualLeadingKnot = if newState.tail.isEmpty then leadingKnot else newState.tail.last
      if followingKnot.isAdjacent(actualLeadingKnot) then newState.copy(tail = newState.tail :+ followingKnot)
      else
        val followingKnotMoveDirection = followingKnot.directionOfOther(actualLeadingKnot)
        val newFollowingKnot = followingKnot.move(followingKnotMoveDirection)
        val updatedTail = newState.tail :+ newFollowingKnot
        val updatedVisited = if updatedTail.size < 9 then newState.visited else newState.visited + newFollowingKnot
        newState.copy(tail = updatedTail, visited = updatedVisited)
    }
    println(newStateFinished)
    newStateFinished
  }

  val result2 = finalLongRopeState.visited.size
  println(result2)

}
