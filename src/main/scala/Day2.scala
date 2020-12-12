import scala.io.Source

object Day2 extends App {
  val input = Source.fromResource("Day2Input.txt").getLines().toList
  val result = input.map(isValidPassword(_, true)).count(_ == true)
  println(result)
  val result2 = input.map(isValidPassword(_, false)).count(_ == true)
  println(result2)

  def isValidPassword(input: String, isPart1: Boolean): Boolean = {
    val splitInput: Array[String] = input.split(" ")
    val minAndMax: Array[Int] = splitInput(0).split("-").map(_.toInt)
    val mustIncludeChar = splitInput(1)(0)
    val password = splitInput(2)

    if (isPart1) {
      val numOfchar = password.count(_ == mustIncludeChar)

      numOfchar >= minAndMax(0) && numOfchar <= minAndMax(1)
    } else {
      minAndMax.count(x => password(x - 1) == mustIncludeChar) == 1
    }
  }
}
