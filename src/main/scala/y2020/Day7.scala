package y2020

import scala.annotation.tailrec
import scala.io.Source

object Day7 extends App {
  val input = Source
    .fromResource("y2020/Day7Input.txt")
    .getLines()
    .map(_.replace("bags", "").replace("bag", "").replace(".", ""))
    .map(_.split("contain"))
    .toList
  val result = get("shiny gold").toList

  println(result.length)

  @tailrec
  def get(
      bag: String,
      bags: Set[String] = Set(),
      toReturn: Set[String] = Set()
  ): Set[String] = {
    val pb = getParentBag(bag.trim)
    val restBags = bags ++ pb

    if (pb.isEmpty && restBags.isEmpty) {
      toReturn
    } else {
      get(restBags.head, restBags.tail, toReturn ++ restBags)
    }
  }

  def getParentBag(b: String): List[String] = {
    input.filter(_(1).contains(b)).map(_(0))
  }

  val input2 = Source
    .fromResource("Day7Input.txt")
    .getLines()
    .map(
      _.replace("bags", "").replace("bag", "").replace(".", "").replace(" ", "")
    )
    .map(_.split("contain"))
    .flatMap(x => Map(x(0).trim -> x(1).split(",").map(_.trim)))
    .toMap

  val a = 0
//  def getContainedBag(b: String): Map[String, Int] = {
//    val containBags = input2(b.replace(" ", "").trim).map()
//  }

  def getBagColor(b: String): String = {
    b.replaceAll("([0-9])", "")
  }

  def getBagCount(b: String): String = {
    b.replaceAll("([a-z])", "")
  }
}
