package ch.hibernator.adventofcode

object Day16 extends SolutionBase {
  override def day: Int = 16

  val lineRegex = "Valve ([A-Z]{2}) has flow rate=([0-9]{1,2}); tunnels? leads? to valves? (.*)".r

  case class Valve(name: String, flowRate: Int, neighbors: Seq[String], open: Boolean)

  val valves: Seq[Valve] = testInput
    .map { line =>
      line match
        case lineRegex(valveName, flowRate, neighbors) =>
          Valve(s"${valveName}closed", flowRate.toInt, neighbors.split(", ").map(name => s"${name}closed"), false)
        case _ => sys.error("Invalid line")
    }
    .flatMap { valve =>
      if valve.flowRate == 0 then Seq(valve)
      else
        Seq(
          valve.copy(neighbors = valve.neighbors :+ valve.name.replace("closed", "open")),
          valve.copy(name = valve.name.replace("closed", "open"), open = true)
        )
    }

  println(valves)
}
