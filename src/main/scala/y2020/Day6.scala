package y2020

import scala.annotation.tailrec
import scala.io.Source

object Day6 extends App {
  val input = Source.fromResource("y2020/Day6Input.txt").getLines()
    .map(x => if (x.isEmpty) " " else x)
    .reduce((acc, cur) => acc + cur)
    .split(" ")

  val result = input.map(countQuestion).sum

  println(result)

  val input2 = Source.fromResource("Day6Input.txt").getLines()
    .map(x => if (x.isEmpty) "-" else x)
    .reduce((acc, cur) => acc + " " + cur)
    .split("-")
    .map(_.trim)
    .map(_.split(" "))

  val result2 = input2.map(x => x.reduce((acc, cur) => acc.intersect(cur))).map(countQuestion).sum

  println(result2)

  def countQuestion(answer: String): Int = {
    @tailrec
    def getQuestions(ans: String, q: String = ""): String = {
      if (ans.isEmpty) {
        q
      } else {
        if (q.contains(ans.head)) {
          getQuestions(ans.tail, q)
        } else {
          getQuestions(ans.tail, q + ans.head)
        }
      }
    }

    getQuestions(answer).length
  }
}
