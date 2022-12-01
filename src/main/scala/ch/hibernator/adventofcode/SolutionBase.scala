package ch.hibernator.adventofcode

import scala.io.{BufferedSource, Source}

trait SolutionBase extends App:
  def day: Int

  val testSource: BufferedSource = Source.fromFile(s"testInput/$day.txt")
  val testInput: Seq[String] = testSource.getLines().toSeq
  testSource.close()

  val source: BufferedSource = Source.fromFile(s"input/$day.txt")
  val input: Seq[String] = source.getLines().toSeq
  source.close()
