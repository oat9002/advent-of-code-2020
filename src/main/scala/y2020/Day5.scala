package y2020

import scala.io.Source

object Day5 extends App {
  val input = Source.fromResource("y2020/Day5Input.txt").getLines().toList
  val allSeat = input.map(x => {
    val result = getRowAndCol(x)
    getSeat(result._1, result._2)
  })
  val highestSeat = max(allSeat)

  println(highestSeat)

  val availableSeats = {
    val rc = input.map(getRowAndCol)
    val firstRow = min(rc.map(_._1))
    val lastRow = max(rc.map(_._1))
    val availableRc = rc.filter(x => x._1 != firstRow && x._1 != lastRow)

    availableRc.map(x => getSeat(x._1, x._2))
  }
  val mySeat = availableSeats.filter(x => !allSeat.contains(x + 1) || !allSeat.contains(x - 1))

  println(mySeat.sum / 2)


  def min(v: List[Int]): Int = {
    v.reduce((acc, cur) => if (cur < acc) cur else acc)
  }

  def max(v: List[Int]): Int = {
    v.reduce((acc, cur) => if (cur > acc) cur else acc)
  }

  def getRowAndCol(code: String): (Int, Int) = {
    val rowCode = code.slice(0, 7)
    val columnCode = code.slice(7, code.length)
    val rowT = rowCode.foldLeft((0, 127, ""))((acc, cur) => cur match {
      case 'F' => (acc._1, (acc._1 + acc._2) / 2, "F")
      case 'B' => (((acc._1 + acc._2) / 2) + 1, acc._2, "B")
    })
    val columnT = columnCode.foldLeft((0, 7, ""))((acc, cur) => cur match {
      case 'L' =>  (acc._1, (acc._1 + acc._2) / 2, "L")
      case 'R' => (((acc._1 + acc._2) / 2) + 1, acc._2, "L")
    })
    val row = rowT._3 match {
      case "F" => rowT._1
      case "B" => rowT._2
    }
    val column = columnT._3 match {
      case "L" => columnT._1
      case "R" => columnT._2
    }

    (row, column)
  }

  def getSeat(row: Int, col: Int): Int = {
    (row * 8) + col
  }
}
