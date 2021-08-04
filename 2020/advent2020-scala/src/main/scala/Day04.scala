// regex interpolation from
// https://github.com/OlegIlyenko/hacking-scala-blog/blob/master/posts/String-Interpolation-Meets-Regular-Expressions.md
implicit class RegexOps(sc: StringContext) {
  def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
}

object Day04 {
  // records delimited by empty line
  // ------
  // ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
  // byr:1937 iyr:2017 cid:147 hgt:183cm
  //
  // iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
  // hcl:#cfa07d byr:1929
  //
  // hcl:#ae17e1 iyr:2013
  // eyr:2024
  // ecl:brn pid:760753108 byr:1931
  // hgt:179cm
  //
  // hcl:#cfa07d eyr:2025 pid:166559648
  // iyr:2011 ecl:brn hgt:59in
  val entry_regex = raw"([a-z]{3}):([a-zA-Z0-9#]+)[ ]?".r

  // byr (Birth Year)
  // iyr (Issue Year)
  // eyr (Expiration Year)
  // hgt (Height)
  // hcl (Hair Color)
  // ecl (Eye Color)
  // pid (Passport ID)
  // cid (Country ID)
  def has_fields(record: Map[String,String]): Boolean =
    List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
      .forall(record.contains(_))

  // byr (Birth Year) - four digits; at least 1920 and at most 2002.
  // iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  // eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  // hgt (Height) - a number followed by either cm or in:
  //     If cm, the number must be at least 150 and at most 193.
  //     If in, the number must be at least 59 and at most 76.
  // hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  // ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  // pid (Passport ID) - a nine-digit number, including leading zeroes.
  // cid (Country ID) - ignored, missing or not.
  def between(x: Int, min:Int, max:Int) = x >= min && x <= max
  def valid_field(x: String): Boolean = {
    x match {
      case r"byr:(\d{4})$n" => between(n.toInt, 1920, 2002)
      case r"iyr:(\d{4})$n" => between(n.toInt, 2010, 2020)
      case r"eyr:(\d{4})$n" => between(n.toInt, 2020, 2030)
      case r"hgt:(\d+)$n(cm|in)$u" => {
        u match {
          case "cm" => between(n.toInt, 150, 193)
          case "in" => between(n.toInt, 59, 76)
        }
      }
      case r"hcl:#[0-9a-f]{6}" => true
      case r"ecl:(amb|blu|brn|gry|grn|hzl|oth)$_" => true
      case r"pid:[0-9]{9}" => true
      case r"cid:.*" => true
      case _ => false
    }
  }

  def valid_record(record: Map[String, String]): Boolean =
    record.forall((k, v: String) => valid_field(s"${k}:${v}"))

  def run(lines: Iterator[String]) = {
    // delimit by empty lines and fold into single map
    var counter = 0
    var valid_counter = 0
    var memo : Map[String, String] = Map()
    for (line <- lines) {
      if (line == "") {
        if (has_fields(memo)) {
          counter += 1
          if (valid_record(memo)) { valid_counter += 1 }
        }
        memo = Map()
      } else {
        val entries = entry_regex.findAllIn(line)
        memo = memo ++ entries.map({
          case entry_regex(k, v) => (k -> v)
        })
      }
    }
    println(s"part1: ${counter}")
    println(s"part2: ${valid_counter}")
  }

}
