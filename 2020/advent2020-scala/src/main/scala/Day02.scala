object Day02 {
  // input layout: "1-3 a: abcde"
  val format = raw"(\d+)-(\d+) ([a-z]): ([a-z]+)".r
  type Entry = (Int, Int, Char, String)


  // Part 1: Password valid if req occurs min..max times in pw
  def part1(entries: List[Entry]): Int =
    entries.filter({(min, max, req, pw) =>
        val count = pw.filter(_ == req).length
        (min <= count && count <= max)
      }).length

  // Part 2: Password valid if req occurs at pw[first] or pw[second] but not both
  // position indexes are 1-based in the input
  def part2(entries: List[Entry]): Int =
    entries.filter({(first, second, req, pw) =>
      if first <= pw.length && second <= pw.length then 
        pw(first-1) == req ^ pw(second-1) == req
      else false
    }).length

  def run(lines: Iterator[String]) = {
    val input = lines.map({
      case format(min, max, req, pw) => (min.toInt, max.toInt, req(0), pw)})
      .toList

    printf("part1: %d\n", part1(input))
    printf("part2: %d\n", part2(input))
  }
}
