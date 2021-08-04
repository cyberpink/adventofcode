@main def main(day: String): Unit =
  day match {
    case "1" => Day01.run(s"day1.txt")
    case "2" => Day02.run(s"day2.txt")
    case _ => println("day not implemented")
  }

