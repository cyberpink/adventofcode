import scala.annotation.tailrec
object Day11 {
  type Board = Array[Char]
  
  def run(lines: Iterator[String]) = {
    val input = lines.toArray
    val w = input(0).length
    val h = input.length
    val data = (for { y <- input; x <- y } yield x).toArray

    def in_bounds(x: Int, y: Int) = x >= 0 && y >= 0 && x < w && y < h

    def get(x: Int, y: Int, storage: Board) =
      if in_bounds(x, y) then
        storage((y * w) + x)
      else
        '.'

    def set(x: Int, y:Int , v: Char, storage: Board) = 
      storage((y * w) + x) = v

    def print_board(b: Board) =
      for (y <- 0 until h)
        println(b.slice(y*w, (y*w)+w).mkString)

    def dirs =
      for {
        dx <- List(-1, 0, 1);
        dy <- List(-1, 0, 1);
        if (dx, dy) != (0, 0)
      } yield (dx, dy)

    def apply_rules(x: Int, y: Int, current: Board, next: Board) = {
      val occupied =
        dirs
          .map({(dx, dy) => get(x+dx, y+dy, current)})
          .filter(_ == '#')
          .length
      get(x, y, current) match {
        case 'L' if occupied == 0 => { set(x, y, '#', next); true }
        case '#' if occupied >= 4 => { set(x, y, 'L', next); true }
        case s => { set(x, y, s, next); false }
      }
    }

    type Stencil = (Int, Int, Board, Board) => Boolean
    @tailrec
    def run(current: Board, next: Board, rules: Stencil): Int = {
      var changed = false
      for (y <- 0 until h)
        for (x <- 0 until w)
          changed = rules(x, y, current, next) || changed

      if changed then
        run(next, current, rules)
      else
        next.filter(_ == '#').length
    }

    printf("part1: %d \n", run(data.clone, data.clone, apply_rules))

    // part 2
    def occupied_in_line(start_x: Int, start_y: Int, dx: Int, dy: Int, data: Board): Boolean = {
      var x: Int = start_x + dx
      var y: Int = start_y + dy
      while(in_bounds(x, y)) {
        get(x, y, data) match {
          case 'L' => return false
          case '#' => return true
          case '.' => {
            x += dx
            y += dy
          }
        }
      }
      return false
    }

    def apply_rules2(x: Int, y: Int, current: Board, next: Board) = {
      val occupied = dirs.filter({(dx, dy) => occupied_in_line(x, y, dx, dy, current) }).length
      get(x, y, current) match {
        case 'L' if occupied == 0 => { set(x, y, '#', next); true }
        case '#' if occupied >= 5 => { set(x, y, 'L', next); true }
        case s => { set(x, y, s, next); false }
      }
    }

    printf("part2: %d \n", run(data.clone, data.clone, apply_rules2))

  }
}
