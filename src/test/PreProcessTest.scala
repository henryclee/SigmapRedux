package test

import helper.fileToMapZ
import org.scalatest._
import preprocess._
import index._

class PreProcessTest extends FunSuite {

  test ("real signal") {

    val filename: String = "data/00.txt"

    val testList: List[Double] = Fast5Process.f5ToListZ(filename)
    for (i <- 0 to 100) {
      //println(testList(i))
    }
  }

  test ("convert to vector") {

    val filename: String = "data/00.txt"
    val dim: Int = 6

    val kmerModel: String = "data/6merModel.txt"
    val kmerMap: Map[String, Double] = fileToMapZ(kmerModel)

    val testArray: Array[Vector] = Fast5Process.fast5ToVector(6, kmerMap, filename, dim)

  }

}
