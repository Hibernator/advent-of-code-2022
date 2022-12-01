package ch.hibernator.adventofcode

object Day1 extends SolutionBase:
  override def day: Int = 1

  val elves = input.foldLeft(Seq[Int](0)) { (acc, line) =>
    if line.isEmpty
    then acc :+ 0
    else acc.updated(acc.size - 1, acc.last + line.toInt)
  }

  val orderedElves = elves.sorted.reverse

  val maxElf = orderedElves.head
  println(maxElf)

  val firstThreeElves = orderedElves.take(3).sum
  println(firstThreeElves)
