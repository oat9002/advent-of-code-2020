package y2020

import scala.io.Source

object Day4 extends App {
  val input = Source.fromResource("y2020/Day4Input.txt").getLines().reduce((acc, cur) => {
    if (cur.isEmpty) {
      acc + "\n"
    } else {
      acc + " " + cur
    }
  }).split("\n")

  val result = input.map(isValid).count(_ == true)
  println(result)

  val result2 = input.map(isValidV2).count(_ == true)
  println(result2)

  def isValid(passport: String): Boolean = {
    val requiredField = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    val fields = passport.trim.split(" ").map(_.split(":")(0))
    val result = requiredField.map(fields.contains(_))
    !result.contains(false)
  }

  def isValidV2(passport: String): Boolean = {
    val requiredField = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    val fields = passport.trim.split(" ").map(_.split(":"))
    val haveAllRequiredFields = requiredField.map(x => fields.map(_(0)).contains(x))

    if (haveAllRequiredFields.contains(false))
      return false

    val isValid = fields.map {
      case x => x(0) match {
        case "byr" =>
          val v = x(1).toInt
          v >= 1920 && v <= 2002
        case "iyr" =>
          val v = x(1).toInt
          v >= 2010 && v <= 2020
        case "eyr" =>  val v = x(1).toInt
          v >= 2020 && v <= 2030
        case "hgt" =>
          val unit = x(1).slice(x(1).length - 2, x(1).length)
          if (!unit.equals("cm") && !unit.equals("in")) {
            false
          } else {
            val h = x(1).slice(0, x(1).length - 2).toInt
            unit match {
              case "cm" => h >= 150 && h <= 193
              case "in" => h >= 59 && h <= 76
              case _ => false
            }
          }
        case "hcl" => x(1).startsWith("#") && x(1).slice(1, x(1).length).matches("^([0-9a-f])+$")
        case "ecl" => List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(x(1))
        case "pid" => x(1).length == 9
        case "cid" => true
        case _ => false
      }
      case _ => false
    }

    !isValid.contains(false)
  }
}
