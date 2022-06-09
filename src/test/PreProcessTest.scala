package test

import org.scalatest._
import preprocess._

class PreProcessTest extends FunSuite {

  test ("random signal") {

    val filename: String = "data/fast5read1.txt"

    val testList: List[Double] = Fast5Process.f5ToListZ(filename)
    println(testList.head)
    println(testList(1))
    println(testList(2))
    println(testList(3))
    println(testList(4))
    println(testList(5))
  }
}
