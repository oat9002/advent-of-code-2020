package y2020

import scala.io.Source

object Day3 extends App {
  val input = Source.fromResource("y2020/Day3Input.txt").getLines().toList

  def countTrees(right: Int, down: Int): BigInt = {
    var columnIdx = right
    var rowIdx = down
    var numOfTrees = 0

    for (i <- input.indices) {
      if (columnIdx >= input(i).length) {
        columnIdx = columnIdx - input(i).length
      }

      if (rowIdx < input.length) {
        val isTree = input(rowIdx)(columnIdx).equals('#')
        numOfTrees = if (isTree) numOfTrees + 1 else numOfTrees
      }

      columnIdx = columnIdx + right
      rowIdx = rowIdx + down
    }

    numOfTrees
  }

  countTrees(1, 2)
  println(s"Result 1: ${countTrees(3, 1)}")
  val result2: BigInt = countTrees(1, 1) * countTrees(3, 1) * countTrees(5, 1) * countTrees(7, 1) * countTrees(1, 2)

  println(s"Result 2: $result2")
}
