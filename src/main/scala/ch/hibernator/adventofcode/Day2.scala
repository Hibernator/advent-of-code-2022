package ch.hibernator.adventofcode

import ch.hibernator.adventofcode.Day2.Item.{Paper, Rock, Scissors}

object Day2 extends SolutionBase {

  override def day: Int = 2

  enum Item(opponentSign: Char, mySign: Char, points: Int):

    def outcome(opponent: Item, me: Item): Outcome =
      opponent match {
        case Rock =>
          me match {
            case Rock => Outcome.Draw
            case Paper => Outcome.Win
            case Scissors => Outcome.Lose
          }
        case Paper =>
          me match {
            case Rock => Outcome.Lose
            case Paper => Outcome.Draw
            case Scissors => Outcome.Win
          }
        case Scissors =>
          me match {
            case Rock => Outcome.Win
            case Paper => Outcome.Lose
            case Scissors => Outcome.Draw
          }
      }

    def forOpponent(sign: Char): Item =

    case Rock extends Item('A', 'X', 1)
    case Paper extends Item('B', 'Y', 2)
    case Scissors extends Item('C', 'Z', 3)

  enum Outcome(points: Int):
    case Lose extends Outcome(0)
    case Draw extends Outcome(3)
    case Win extends Outcome(6)
}
