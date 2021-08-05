object Day06 {
  def run(lines: Iterator[String]) = {
    var any_sum = 0
    var all_sum = 0
    var any: Set[Char] = Set()
    var all: Option[Set[Char]] = None
    for (line <- lines) {
      if (line == "") {
        any_sum += any.size;
        all_sum += all.get.size;
        any = Set()
        all = None
      } else {
        any = any ++ line.toSet
        all = all match {
          case None => Some(line.toSet)
          case Some(s) => Some(s.intersect(line.toSet))
        }
      }
    }
    any_sum += any.size;
    all_sum += all.get.size;
    printf("part1: %d\n", any_sum)
    printf("part2: %d\n", all_sum)
  }
}
