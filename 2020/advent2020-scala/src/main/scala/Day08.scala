object Day08 {
  def run_no_loop(program: Array[(Int, Int) => (Int, Int)]): (Int, Boolean) = {
    var len = program.length
    var state = (0, 0)
    var memo = Set.empty[Int]
    while(!memo.contains(state(0))) {
      state match {
        case (pc, acc) if pc == len =>
          return (acc, true)
        case (pc, acc) if pc >= len || pc < 0 =>
          return (acc, false)
        case (pc, acc) => {
          memo = memo + pc
          state = program(pc)(pc, acc)
        }
      }
    }
    return (state(1), false)
  }

  val instr = raw"(acc|jmp|nop) ([\+\-][0-9]+)".r
  def run(lines: Iterator[String]) = {
    val instructions = lines.map({
      case instr(op, arg) => (op, arg.toInt)
    }).toArray

    // part 1:
    // compile to state transitions and use a trampoline kernel to run
    val p1 = instructions.map({
      case ("acc", arg) =>
        (pc: Int, acc: Int) => (pc + 1, acc + arg)
      case ("jmp", arg) =>
        (pc: Int, acc: Int) => (pc + arg, acc)
      case("nop", arg) =>
        (pc: Int, acc: Int) => (pc + 1, acc)
    })
    val (p1r, _) = run_no_loop(p1)
    printf("part1: %d\n", p1r)

    // part 2:
    // just compile with support for flipping a specific op and brute force flips
    var flip = 0
    val p2 = instructions.map({
      case ("acc", arg) =>
        (pc: Int, acc: Int) => (pc + 1, acc + arg)
      case ("jmp", arg) =>
        (pc: Int, acc: Int) => if pc == flip then (pc + 1, acc) else (pc + arg, acc)
      case("nop", arg) =>
        (pc: Int, acc: Int) => if pc == flip then (pc + arg, acc) else (pc + 1, acc)
    })

    for(i <- 0 until instructions.length) {
      flip = i;
      val (r, t) = run_no_loop(p2)
      if t then printf("part2: %d\n", r)
    }
  }
}
