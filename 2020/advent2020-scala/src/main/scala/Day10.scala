import scala.collection.mutable.ArrayBuffer
object Day10 {
  def run(lines: Iterator[String]) = {
    // part 1:
    // count any time an element has a difference of 1 or 3 from another
    val data = lines.map(_.toInt).toArray.sortInPlace
    var ones = 0
    var threes = 1

    if (data(0) == 1) ones += 1
    if (data(0) == 3) ones += 1
    for (i <- 1 until data.length) {
      val diff = data(i) - data(i-1)
      if (diff == 1) ones += 1
      if (diff == 3) threes += 1
    }
    printf("part1: %d\n", ones * threes)

    // part 2:
    // find distinct number of arrangements

    // number of combinations = sum of tail combinations
    var memo : Map[Int, BigInt] = Map()
    def num_tails(i: Int): BigInt =
      memo.get(i) match {
        case Some(n) => n
        case None => {
          val tails =
            Range((i + 1), Math.min(i + 4, data.length))
              .filter((j) => (data(j) - data(i)) <= 3)
              .map(num_tails)
              .sum
          memo = memo + (i -> tails)
          tails
        }
      }

    var out : BigInt = 0
    memo = memo + ((data.length - 1) -> 1)
    printf("part2: %d\n",
      Range(0, Math.min(3, data.length))
      .filter((j) => data(j) <= 3)
      .map(num_tails)
      .sum)

  }
}
