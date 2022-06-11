package test

import org.scalatest._
import preprocess._

class PreProcessTest extends FunSuite {

  test ("real signal") {

    val filename: String = "data/00.txt"

    val testList: List[Double] = Fast5Process.f5ToListZ(filename)
    for (i <- 0 to 100) {
      println(testList(i))
    }
  }
}
