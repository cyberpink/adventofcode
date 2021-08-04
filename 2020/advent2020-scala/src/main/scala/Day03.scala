object Day03 {
  // Part 1: Count number of trees on slope from (0, 0) (top left) to bottom of map
  // map repeats horizontally, can use modulo on x axis for that
  def part1(slope: (Int, Int), map: Array[String]): Int = {
    val (slope_x, slope_y) = slope
    val map_height = map.length
    val map_width = map(0).length

    var count : Int = 0
    for (y <- Range(0, map_height, slope_y)) {
      val x = slope_x * (y / slope_y)
      if map(y)(x % map_width) == '#' then
        count += 1
    }
    count
  }

  // Part 2: check the following slopes and take the product of their collisions
  //
  // Right 1, down 1.
  // Right 3, down 1. (This is the slope you already checked.)
  // Right 5, down 1.
  // Right 7, down 1.
  // Right 1, down 2.
  def part2(map: Array[String]) : BigInt =
    List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
      .map(part1(_, map)).map(BigInt(_)).reduce(_*_)

  def run(lines: Iterator[String]) = {
    val input = lines.toArray
    printf("part1: %d\n", part1((3, 1), input))
    printf("part2: %d\n", part2(input))
  }
      
}
