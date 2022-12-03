package ch.hibernator.adventofcode

object Day3 extends SolutionBase:
  override def day: Int = 3

  private val lowerCase = ('a' to 'z').toSet
  private val upperCase = ('A' to 'Z').toSet

  extension (character: Char) {
    def toPriority: Int =
      if lowerCase.contains(character)
        then character.toInt - 96
      else character.toInt - 38
  }

  private def priorityFromLine(line: String): Int =
    val (firstHalf, secondHalf) = line.splitAt(line.length / 2)
    firstHalf.toSet.intersect(secondHalf.toSet).head.toPriority

  val sumPriorities = input.map(priorityFromLine).sum
  println(sumPriorities)

  val sumPriorities2 = input
    .grouped(3)
    .map { group =>
      group.head.toSet.intersect(group(1).toSet).intersect(group.last.toSet).head
    }
    .map(_.toPriority)
    .sum

  println(sumPriorities2)
