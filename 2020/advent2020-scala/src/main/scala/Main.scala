import scala.io.Source

def lines(filename: String) = Source.fromResource(filename).getLines


@main def main(day: String): Unit =
  day match {
    case "1" => Day01.run(lines("day1.txt"))
    case "2" => Day02.run(lines("day2.txt"))
    case "3" => Day03.run(lines("day3.txt"))
    case "4" => Day04.run(lines("day4.txt"))
    case "5" => Day05.run(lines("day5.txt"))
    case "6" => Day06.run(lines("day6.txt"))
    case _ => println("day not implemented")
  }
