import scala.io.Source
import scala.collection.mutable.Map

object Day01 {
  def read(file: String) =
    Source.fromResource(file).getLines.map(_.toInt).toList

  // Part 1: find the two entries that sum to 2020
  //
  // a + b = target ==> b = target - a
  //
  // iterate numbers and store b -> a mapping while looking for existing match
  def part1[A](target: Int, input: List[Int]): Option[Int] = {
    val memo: Map[Int, Int] = Map()
    for (a <- input) {
      memo.get(a) match
        case Some(b) => return Some(a * b)
        case None => memo += (target - a -> a)
    }
    return None
  }

  // Part 2: find three entries that sum to 2020
  //
  // a + b + c = target ==> b + c = target - a
  //
  // 1. for every a: store (total - a) -> a
  // 2. iterate every unique combination of two entries as (b, c)
  //    look for matching entry that is different from b & c
  def part2(target: Int, input: List[Int]): Option[Int] = {
    val memo: Map[Int, Int] = Map()
    for(a <- input) do memo += (target - a -> a)
    for(b <- input; c <- input if b != c) {
      memo.get(b + c) match {
        case Some(a) if (a != b && a != c) =>
          return Some(a * b * c)
        case _ => ()
      }
    }
    return None
  }

  def run(file: String) = {
    part1(2020, read(file)) match {
      case None => printf("part1: failed\n")
      case Some(r) => printf("part1: %d\n", r)
    }
    part2(2020, read(file)) match {
      case None => printf("part2: failed\n")
      case Some(r) => printf("part2: %d\n", r)
    }
  }
      
}
