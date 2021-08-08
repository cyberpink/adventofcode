import scala.collection.mutable.Map
import scala.annotation.tailrec

object Day09 {


  // solution of day 1 to find two numbers from a list that add to a target
  def day1(target: Long, input: Array[Long]): Option[Long] = {
    val memo: Map[Long, Long] = Map()
    for (a <- input) {
      memo.get(a) match
        case Some(b) => return Some(a * b)
        case None => memo += (target - a -> a)
    }
    return None
  }

  // XMAS starts by transmitting a preamble of 25 numbers.
  // After that, each number you receive should be the sum of any two of
  // the 25 immediately previous numbers. The two numbers will have different values,
  // and there might be more than one such pair.

  // part 1:
  // find the first number in the list (after the preamble) which
  // is not the sum of two of the 25 numbers before it.
  def part1(data: Array[Long]): Option[Long] = {
    for(i <- 25 until data.length - 1) {
      day1(data(i), data.slice(i-25, i)) match
      case None => return Some(data(i))
      case Some(_) => ()
    }
    return None
  }

  // loop from the start until the end
  // building up a list of numbers, fail if the list exceeds
  def build_window(target: Long, start: Int, data : Array[Long]): Option[Int] = {
    var sum = data(start)
    for(i <- start + 1 until data.length - 1) {
      sum += data(i)
      if (sum == target) return Some(i)
      if (sum > target) return None
    }
    None
  }

  // part 2:
  // find a contiguous set of at least two numbers in your list
  // which sum to the invalid number from step 1.
  def part2(target: Long, data: Array[Long]): Option[Long] = {
    for(i <- 0 until data.length - 1) {
      build_window(target, i, data) match {
        case None => ()
        case Some(j) => {
          val sub = data.slice(i, j+1)
          return Some(sub.min + sub.max)
        }
      }
    }
    None
  }

  def run(lines: Iterator[String]) = {
    val data = lines.map(_.toLong).toArray
    val p1 = part1(data).get
    println(p1)
    println(part2(p1, data).get)
  }
}
