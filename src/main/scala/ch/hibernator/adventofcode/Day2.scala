package ch.hibernator.adventofcode

import ch.hibernator.adventofcode.Day2.Item.{Paper, Rock, Scissors}

object Day2 extends SolutionBase:

  override def day: Int = 2

  enum Item(opponentSign: Char, mySign: Char, val points: Int):

    case Rock extends Item('A', 'X', 1)
    case Paper extends Item('B', 'Y', 2)
    case Scissors extends Item('C', 'Z', 3)

  object Item:
    def outcome(opponent: Item, me: Item): Outcome =
      opponent match
        case Rock =>
          me match
            case Rock => Outcome.Draw
            case Paper => Outcome.Win
            case Scissors => Outcome.Lose
        case Paper =>
          me match
            case Rock => Outcome.Lose
            case Paper => Outcome.Draw
            case Scissors => Outcome.Win
        case Scissors =>
          me match
            case Rock => Outcome.Win
            case Paper => Outcome.Lose
            case Scissors => Outcome.Draw

    def forOpponent(sign: Char): Item =
      sign match
        case 'A' => Rock
        case 'B' => Paper
        case 'C' => Scissors

    def forMe(sign: Char): Item =
      sign match
        case 'X' => Rock
        case 'Y' => Paper
        case 'Z' => Scissors

  enum Outcome(val points: Int, sign: Char):
    case Lose extends Outcome(0, 'X')
    case Draw extends Outcome(3, 'Y')
    case Win extends Outcome(6, 'Z')

  object Outcome:

    def forSign(sign: Char): Outcome =
      sign match
        case 'X' => Lose
        case 'Y' => Draw
        case 'Z' => Win

    def getMatchingItem(opponentItem: Item, desiredOutcome: Outcome): Item =
      opponentItem match
        case Rock =>
          desiredOutcome match
            case Lose => Scissors
            case Draw => Rock
            case Win => Paper
        case Paper =>
          desiredOutcome match
            case Lose => Rock
            case Draw => Paper
            case Win => Scissors
        case Scissors =>
          desiredOutcome match
            case Lose => Paper
            case Draw => Scissors
            case Win => Rock

  private val totalPoints = input
    .map(line => (Item.forOpponent(line.head), Item.forMe(line.last)))
    .map { (opponentItem, myItem) =>
      myItem.points + Item.outcome(opponentItem, myItem).points
    }
    .sum

  println(totalPoints)

  private val totalPoints2 = input
    .map { line =>
      val opponentItem = Item.forOpponent(line.head)
      val desiredOutcome = Outcome.forSign(line.last)
      val myItem = Outcome.getMatchingItem(opponentItem, desiredOutcome)
      myItem.points + desiredOutcome.points
    }
    .sum

  println(totalPoints2)