import scala.io.Source
import scala.collection.mutable.Map

object Day01 {
  def read(file: String) =
    Source.fromResource(file).getLines.map(_.toInt).toList

  // a + b = target
  // b = target - a
  def part1[A](target: Int, input: List[Int]): Option[Int] = {
    val memo: Map[Int, Int] = Map()
    for (a <- input) {
      memo.get(a) match
        // found pairing
        case Some(b) => return Some(a * b)
        // leave note for desired pair number
        case None => memo += (target - a -> a)
    }
    return None
  }

  // a + b + c = target
  // b + c = target - a
  def part2(target: Int, input: List[Int]): Option[Int] = {
    val memo: Map[Int, Int] = Map()
    // store desired pairing for every number
    for(a <- input) do memo += (target - a -> a)
    // look for match in every sum combination where all 3 numbers are unique
    for(a <- input; b <- input if a != b) {
      memo.get(a + b) match {
        case Some(c) if (c != a && c != b) =>
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
