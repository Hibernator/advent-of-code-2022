package ch.hibernator.adventofcode

import scala.collection.mutable

object Day11 extends SolutionBase {
  override def day: Int = 11

  case class Operation(numericalOperation: Char, secondOperand: String):
    def execute(worryLevel: Long): Long = {
      val second = if secondOperand == "old" then worryLevel else secondOperand.toLong
      val inspectionWorryLevel = if numericalOperation == '+' then worryLevel + second else worryLevel * second
//      inspectionWorryLevel / 3L
      inspectionWorryLevel
    }

  case class Throwing(divideBy: Long, monkeyIfTrue: Int, monkeyIfFalse: Int):
    def execute(worryLevel: Long): Int = if worryLevel % divideBy == 0 then monkeyIfTrue else monkeyIfFalse

  class Monkey(
      val items: mutable.Buffer[Long],
      val operation: Operation,
      val throwing: Throwing,
      var itemsInspected: Long = 0L
  )

  val monkeys = input
    .grouped(7)
    .map { rawMonkey =>
      val startingItems = rawMonkey(1).split(": ").last.split(", ").map(_.toLong).to(mutable.Buffer)
      val rawOperation = rawMonkey(2).split(" = old ").last.split(" ")
      val operation = Operation(rawOperation.head.head, rawOperation.last)
      val divideBy = rawMonkey(3).split(" ").last.toLong
      val monkeyIfTrue = rawMonkey(4).last.toString.toInt
      val monkeyIfFalse = rawMonkey(5).last.toString.toInt
      val throwing = Throwing(divideBy, monkeyIfTrue, monkeyIfFalse)
      Monkey(startingItems, operation, throwing)
    }
    .toSeq

  val divideByProduct = monkeys.map(_.throwing.divideBy).product

  def processMonkey(index: Int): Unit = {
    val monkey = monkeys(index)
    monkey.items.foreach { item =>
      val finalWorryLevel = monkey.operation.execute(item)
      val monkeyToThrow = monkeys(monkey.throwing.execute(finalWorryLevel))
      monkeyToThrow.items.addOne(finalWorryLevel % divideByProduct)
      monkey.itemsInspected = monkey.itemsInspected + 1L
    }
    monkey.items.clear()
  }

  for (
//    _ <- 1 to 20;
    _ <- 1 to 10000;
    monkeyIndex <- monkeys.indices
  )
  do processMonkey(monkeyIndex)

  val result = monkeys.sortBy(_.itemsInspected).reverse.map(_.itemsInspected).take(2).product

  println(result)
}
