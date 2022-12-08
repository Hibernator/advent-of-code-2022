package ch.hibernator.adventofcode

import scala.annotation.tailrec
import scala.collection.mutable

object Day8 extends SolutionBase {
  override def day: Int = 8

  val realInput = input

  val numRows = realInput.size
  val numCols = realInput.head.length

  class Matrix(rows: Int, cols: Int):
    private val treesRowsFirst = Array.fill(rows)(Array.fill(cols)(0))
    private val treesColsFirst = Array.fill(cols)(Array.fill(rows)(0))

    def fillTrees(rawInput: Seq[String]): Unit =
      for
        row <- 0 until rows
        col <- 0 until cols
      do
        val tree = rawInput(row)(col).toString.toInt
        treesRowsFirst(row)(col) = tree
        treesColsFirst(col)(row) = tree

    def isVisible(row: Int, col: Int): Boolean = {
      if row == 0 || col == 0 || row == rows - 1 || col == cols - 1 then true
      else isVisibleFromLeft(row, col) || isVisibleFromRight(row, col) || isVisibleFromTop(row, col) ||
        isVisibleFromBottom(row, col)
    }

    private def findSameOrHigherOnLeft(row: Int, col: Int): Option[Int] = {
      val currentRow = treesRowsFirst(row)
      val currentTree = currentRow(col)
      currentRow.take(col).find(_ >= currentTree)
    }

    private def findSameOrHigherOnRight(row: Int, col: Int): Option[Int] = {
      val currentRow = treesRowsFirst(row)
      val currentTree = currentRow(col)
      currentRow.drop(col + 1).find(_ >= currentTree)
    }

    private def findSameOrHigherOnTop(row: Int, col: Int): Option[Int] = {
      val currentCol = treesColsFirst(col)
      val currentTree = currentCol(row)
      currentCol.take(row).find(_ >= currentTree)
    }

    private def findSameOrHigherOnBottom(row: Int, col: Int): Option[Int] = {
      val currentCol = treesColsFirst(col)
      val currentTree = currentCol(row)
      currentCol.drop(row + 1).find(_ >= currentTree)
    }

    private def isVisibleFromLeft(row: Int, col: Int): Boolean = findSameOrHigherOnLeft(row, col).isEmpty
    private def isVisibleFromRight(row: Int, col: Int): Boolean = findSameOrHigherOnRight(row, col).isEmpty
    private def isVisibleFromTop(row: Int, col: Int): Boolean = findSameOrHigherOnTop(row, col).isEmpty
    private def isVisibleFromBottom(row: Int, col: Int): Boolean = findSameOrHigherOnBottom(row, col).isEmpty

    def visibilityScore(row: Int, col: Int): Int = {
      treesVisibleOnLeft(row, col) *
        treesVisibleOnRight(row, col) *
        treesVisibleOnTop(row, col) *
        treesVisibleOnBottom(row, col)
    }

    private def treesVisibleOnLeft(row: Int, col: Int): Int = {
      val currentRow = treesRowsFirst(row)
      val treesOnLeft = currentRow.take(col)
      treesVisibleFromEnd(currentRow(col), treesOnLeft)
    }

    private def treesVisibleOnRight(row: Int, col: Int): Int = {
      val currentRow = treesRowsFirst(row)
      val treesOnRight = currentRow.drop(col + 1)
      treesVisibleFromBeginning(currentRow(col), treesOnRight)
    }

    private def treesVisibleOnTop(row: Int, col: Int): Int = {
      val currentCol = treesColsFirst(col)
      val treesOnTop = currentCol.take(row)
      treesVisibleFromEnd(currentCol(row), treesOnTop)
    }

    private def treesVisibleOnBottom(row: Int, col: Int): Int = {
      val currentCol = treesColsFirst(col)
      val treesOnBottom = currentCol.drop(row + 1)
      treesVisibleFromBeginning(currentCol(row), treesOnBottom)
    }

    private def treesVisibleFromEnd(currentTree: Int, trees: Array[Int]) =
      treesVisibleFromBeginning(currentTree, trees.reverse)
    private def treesVisibleFromBeginning(currentTree: Int, trees: Array[Int]): Int = {
      @tailrec
      def recursive(treesSoFar: Int, remainingTrees: Array[Int]): Int = {
        if remainingTrees.isEmpty then treesSoFar
        else if remainingTrees.head >= currentTree then treesSoFar + 1
        else recursive(treesSoFar + 1, remainingTrees.tail)
      }

      recursive(0, trees)
    }

  val trees = new Matrix(numRows, numCols)
  trees.fillTrees(realInput)

  println(trees)

  val result = (for
    row <- 0 until numRows
    col <- 0 until numCols
  yield trees.isVisible(row, col)).count(_ == true)
  println(result)

  val coordinatesToVisibility: mutable.Map[(Int, Int), Int] = mutable.Map()
  for
    row <- 0 until numRows
    col <- 0 until numCols
  do
    coordinatesToVisibility.addOne((row, col), trees.visibilityScore(row, col))

  val result2 = coordinatesToVisibility.values.max
  println(result2)

}
