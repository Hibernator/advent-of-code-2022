package ch.hibernator.adventofcode

import scala.collection.mutable

object Day5 extends SolutionBase:
  override def day: Int = 5

  case class Move(amount: Int, from: Int, to: Int):
    def move9000(state: mutable.Map[Int, mutable.Stack[Char]]): Unit = {
      val (fromStack, toStack) = (state(from), state(to))
      for (_ <- 1 to amount) {
        toStack.push(fromStack.pop())
      }
    }

    def move9001(state: mutable.Map[Int, mutable.Stack[Char]]): Unit = {
      val (fromStack, toStack) = (state(from), state(to))
      toStack.pushAll(fromStack.take(amount).reverse)
      for (_ <- 1 to amount) {
        fromStack.pop()
      }
    }

  case object Move:
    def parse(raw: String): Move = {
      val numbers =
        raw.replace("move", "").replace("from", "").replace("to", "").split(" ").filter(_.nonEmpty).map(_.toInt)
      Move(numbers.head, numbers(1), numbers.last)
    }

  // constructing the initial state
  val (rawInitialState, rawMoves) = (input.takeWhile(_.nonEmpty), input.dropWhile(!_.startsWith("m")))
  val moves = rawMoves.map(Move.parse)
  val state: mutable.Map[Int, mutable.Stack[Char]] = mutable.Map()
  val (rawStacks, rawStackNumbers) = (rawInitialState.init, rawInitialState.last)
  val stackNumbers = rawStackNumbers.split(" ").filter(_.nonEmpty).map(_.toInt)
  val stackPositions =
    rawStackNumbers
      .split(" ")
      .filter(_.nonEmpty)
      .map(number => rawStackNumbers.indexWhere(_ == number.toCharArray.head))
  val stackPositionToStackNumber = stackPositions.zip(stackNumbers).toMap
  stackNumbers.foreach(number => state.addOne(number, mutable.Stack()))
  rawStacks.reverse.foreach { rawStack =>
    val paddedRawStack = rawStack.padTo(200, ' ')
    stackPositions.foreach { stackPosition =>
      val maybeCrate = paddedRawStack(stackPosition)
      if maybeCrate != ' ' then state(stackPositionToStackNumber(stackPosition)).push(maybeCrate)
    }
  }

  // applying the moves
  // move9000 is used for the first half of the puzzle and move9001 for the second half
  moves.foreach(_.move9001(state))

  val result = stackNumbers.foldLeft("") { (acc, stackNumber) =>
    acc.appended(state(stackNumber).head)
  }
  println(result)
