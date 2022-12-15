package ch.hibernator.adventofcode

import ch.hibernator.adventofcode.Day13.TestResult
import ch.hibernator.adventofcode.Day13.TestResult.{Correct, Undecided, Wrong}

import scala.annotation.tailrec
import scala.collection.mutable

object Day13 extends SolutionBase {
  override def day: Int = 13

  val realInput = input

  enum TestResult:
    case Correct extends TestResult
    case Wrong extends TestResult
    case Undecided extends TestResult

  trait Data:
    def testAgainst(other: Data): TestResult

  case class SingleValue(number: Int) extends Data:
    override def testAgainst(other: Data): TestResult =
      other match
        case SingleValue(otherNumber: Int) =>
          if number < otherNumber then Correct
          else if number == otherNumber then Undecided
          else Wrong
        case list: ValueList =>
          ValueList(this.number).testAgainst(list)

    override def toString: String = number.toString

  case class ValueList(innerData: mutable.Buffer[Data]) extends Data:
    override def testAgainst(other: Data): TestResult = {
      val otherList = other match
        case SingleValue(otherNumber) => ValueList(otherNumber).innerData
        case valueList: ValueList     => valueList.innerData

      val intermediateResult =
        innerData.zip(otherList).foldLeft(Undecided) { case (currentResult, (thisData, otherData)) =>
          val result = thisData.testAgainst(otherData)
          (currentResult, result) match
            case (Wrong, _)             => Wrong
            case (Correct, _)           => Correct
            case (Undecided, newResult) => newResult
        }

      intermediateResult match {
        case Undecided =>
          val thisSize = innerData.size
          val otherSize = otherList.size
          if thisSize < otherSize then Correct
          else if thisSize > otherSize then Wrong
          else Undecided
        case decided => decided
      }
    }

    override def toString = s"[${innerData.map(_.toString).mkString(",")}]"

  object ValueList:
    def apply(number: Int): ValueList = ValueList(mutable.Buffer(SingleValue(number)))
    def apply(): ValueList = ValueList(mutable.Buffer())

  def parseLine(line: String): ValueList = {
    var currentList = ValueList()
    val outerLists: mutable.Stack[ValueList] = mutable.Stack()

    line.zipWithIndex.foreach { (character, index) =>
      character match
        case '[' =>
          val newList = ValueList()
          currentList.innerData.addOne(newList)
          outerLists.push(currentList)
          currentList = newList
        case ']' => currentList = outerLists.pop()
        case num if num.toString.toIntOption.isDefined && line(index + 1).toString.toIntOption.isEmpty =>
          currentList.innerData.addOne(SingleValue(num.toString.toInt))
        case '1' if line(index + 1) == '0' => currentList.innerData.addOne(SingleValue(10))
        case '0' if line(index - 1) == '1' =>
        case _                             =>
    }
    currentList
  }

  val listPairs = realInput
    .grouped(3)
    .map { lines =>
      (parseLine(lines.head), parseLine(lines(1)))
    }
    .toSeq

  val result = listPairs
    .map(pair => pair._1.testAgainst(pair._2))
    .zipWithIndex
    .filter { (result, _) => result == Correct }
    .map(_._2 + 1)
    .sum

  println(result)

  object DataOrdering extends Ordering[Data]:
    override def compare(x: Data, y: Data): Int = if x.testAgainst(y) == Correct then -1 else 1

  val firstDivider = ValueList(innerData = mutable.Buffer(ValueList(2)))
  val secondDivider = ValueList(innerData = mutable.Buffer(ValueList(6)))
  val allPackets = listPairs.flatMap(pair => Seq(pair._1, pair._2)) ++ Seq(firstDivider, secondDivider)

  val orderedPackets = allPackets.sorted(DataOrdering)
  val result2 = (orderedPackets.indexOf(firstDivider) + 1) * (orderedPackets.indexOf(secondDivider) + 1)
  println(result2)
}
