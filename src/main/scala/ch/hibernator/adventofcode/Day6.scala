package ch.hibernator.adventofcode

object Day6 extends SolutionBase {
  override def day: Int = 6

  val stream = input.head

  val packetStart = stream.sliding(4).find(_.toSet.size == 4)
  val result1 = stream.indexOfSlice(packetStart.get.toCharArray) + 4
  println(result1)

  val messageStart = stream.sliding(14).find(_.toSet.size == 14)
  val result2 = stream.indexOfSlice(messageStart.get.toCharArray) + 14
  println(result2)
}
