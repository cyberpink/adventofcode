@main def main(day: String): Unit =
  day match {
    case "1" => Day01.run(s"day1.txt")
    case "2" => Day02.run(s"day2.txt")
    case "3" => Day03.run(s"day3.txt")
    case _ => println("day not implemented")
  }

