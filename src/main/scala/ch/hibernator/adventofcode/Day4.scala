package ch.hibernator.adventofcode

object Day4 extends SolutionBase:
  override def day: Int = 4

  extension (range: Range) {
    def encompasses(other: Range): Boolean =
      range.min <= other.min && range.max >= other.max

    def overlaps(other: Range): Boolean =
      range.max >= other.min && other.max >= range.min
  }

  val rangePairs = input
    .map { line =>
      val ranges = line.split(",").map { rawRange =>
        val boundaries = rawRange.split("-").map(_.toInt)
        boundaries.head to boundaries.last
      }
      (ranges.head, ranges.last)
    }

  val fullyContainingNum = rangePairs
    .map { (firstRange, secondRange) =>
      firstRange.encompasses(secondRange) || secondRange.encompasses(firstRange)
    }
    .count(_ == true)

  println(fullyContainingNum)

  val overlappingNum = rangePairs
    .map { (firstRange, secondRange) =>
      firstRange.overlaps(secondRange)
    }
    .count(_ == true)

  println(overlappingNum)
