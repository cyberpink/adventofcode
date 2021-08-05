object Day05 {

  // BSP format "FBFBBFFRLR" (F|B){7}(L|R){3}
  // F = Lower; B = Higher
  // L = Lower; R = Higher;
  def decodeFB(path: String): Int =
    path.foldLeft(0)({
      case (i, 'F') => (i << 1) | 0
      case (i, 'B') => (i << 1) | 1
    })

  def decodeLR(path: String): Int =
    path.foldLeft(0)({
      case (i, 'L') => (i << 1) | 0
      case (i, 'R') => (i << 1) | 1
    })

  def calc_id(row: Int, col: Int) = (row * 8) + col

  def run(lines: Iterator[String]) = {
    var highest = 0
    var set: Set[Int] = Set()
    for (line <- lines) {
      val row = decodeFB(line.slice(0, 7))
      val col = decodeLR(line.slice(7,10))
      val id = calc_id(row, col)
      set = set + id
      highest = highest.max(id)
    }
    println(s"part1: ${highest}")
    for
      row <- 1 until 128
      col <- 1 until 8
    do
      val i = calc_id(row, col)
      if (!(set.contains(i)) && set.contains(i-1) && set.contains(i+1)) then
        println(s"part2: ${i}")
    }
}
