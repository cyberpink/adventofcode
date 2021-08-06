object Day07 {
  def reachable(init: String, edgemap: Map[String, Set[String]]): Set[String] =
    edgemap.get(init) match
      case None => Set()
      case Some(containers) => containers.map(reachable(_, edgemap)).foldLeft(containers)(_++_)


  // Get the total sub-bag count for every bag type
  // Starting from a list of bags that contain no other:
  //   1. propagate values of unit clauses (bags that have their sub-bag count fully computed)
  //   2. continue with any new unit clauses created during reduction
  @scala.annotation.tailrec
  def calc(
    queue: List[String],
    child_map: Map[String, Map[String, BigInt]],
    parent_map: Map[String, Set[String]],
    sum_map: Map[String, BigInt])
      : Map[String, BigInt] =
  {
    queue match
    case Nil => sum_map
    case color :: qrest => {
      val amount = sum_map.getOrElse(color, BigInt(1))
      val parents = parent_map.getOrElse(color, Set()).toList
      val (queue1, child_map1, sum_map1) =
        parents.foldLeft((qrest, child_map, sum_map))
          ((memo, parent) => {
            val (queue, child_map, sum_map) = memo
            val children: Map[String, BigInt] = child_map.getOrElse(parent, Map())
            val full_amount = amount * children.getOrElse(color, BigInt(0))
            val sum_map1 = sum_map.updatedWith(parent)((sum) => Some(full_amount + sum.getOrElse(1)))

            (children - color) match 
              case children1 if children1.isEmpty =>
                (parent :: queue, child_map - parent, sum_map1)
              case children1 =>
                (queue, child_map.updated(parent, children1), sum_map1)            
          })
      calc(queue1, child_map1, parent_map, sum_map1)
    }
  }

  // examples:
  // light red bags contain 1 bright white bag, 2 muted yellow bags.
  // dark violet bags contain no other bags.
  def run(lines: Iterator[String]) = {
    var parents: Map[String, Set[String]] = Map()
    var children: Map[String, Map[String, BigInt]] = Map()
    var units: List[String] = List()
    for(line <- lines) {
      val one = line.split(" bags contain ")
      val container = one(0)
      val two = one(1)
        .replace(" bags", "")
        .replace(" bag", "")
        .replace(".", "")
      two match {
        case "no other" => units = (container :: units)
        case s =>
          s.split(", ")
            .foreach((x) => {
              val n: BigInt = x.slice(0,1).toInt
              val color = x.slice(2, x.length)
              parents = parents.updatedWith(color)((ps) =>
                Some(ps.getOrElse(Set()) + container))
              children = children.updatedWith(container)((cs) =>
                Some(cs.getOrElse(Map()) + (color -> n)))
            })
      }
    }
    printf("part1: %d\n", reachable("shiny gold", parents).size)
    printf("part2: %d\n", calc(units, children, parents, Map())("shiny gold")-1)
  }
}
