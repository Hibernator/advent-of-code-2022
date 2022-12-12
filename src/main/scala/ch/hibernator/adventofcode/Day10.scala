package ch.hibernator.adventofcode

import scala.collection.mutable

object Day10 extends SolutionBase {
  override def day: Int = 10

  val cycles = input
    .map { line =>
      val lineSplit = line.split(" ")
      lineSplit.head match
        case "noop" => 0
        case "addx" => lineSplit.last.toInt
    }
    .flatMap { number =>
      number match
        case 0       => Seq(0)
        case nonZero => Seq(0, nonZero)
    }

  val neededCycles: mutable.Buffer[(Int, Int)] = mutable.Buffer()

  (1 +: cycles).zipWithIndex.foldLeft(0) { (currentRegister, pair) =>
    val (toAdd, index) = (pair._1, pair._2)
    val newRegister = currentRegister + toAdd
    if index % 40 == 20 then neededCycles.addOne((index, currentRegister))
    newRegister
  }

  val result = neededCycles.map(pair => pair._1 * pair._2).sum
  println(result)

  cycles.zipWithIndex.foldLeft(1) { (currentMiddlePixelPosition, pair) =>
    val (toAdd, cycleNum) = (pair._1, pair._2 + 1)
    val currentPixelToDraw = (cycleNum - 1) % 40
    if currentPixelToDraw >= currentMiddlePixelPosition - 1 && currentPixelToDraw <= currentMiddlePixelPosition + 1
    then print("#")
    else print(".")
    if cycleNum % 40 == 0 then println()
    currentMiddlePixelPosition + toAdd
  }
}
