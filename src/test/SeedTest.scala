package test

import org.scalatest._
import seed.Seeding
import index.Vector
import index.EventSeqVector._
import helper._

class SeedTest extends FunSuite {

  test("Seed scoring") {
    val testSeed1: Vector = new Vector(0, Array(1, 1, 1, 1))
    println(Seeding.seedScore(testSeed1))
    val testSeed2: Vector = new Vector(0, Array(1, 0, 1, 1))
    println(Seeding.seedScore(testSeed2))
  }

  test ("Select top seeds") {

    val numSeeds: Int = 10
    var topSeeds: Array[Vector] = new Array[Vector](numSeeds)

    val kmerModel: String = "data/6merModel.txt"
    val kmerMap: Map[String, Double] = fileToMapZ(kmerModel)

    val filename: String = "data/Euglena gracilis half.fna"

    var testList: List[Char] = readBasesList(filename)
    var eventArray: Array[Vector] = listToVector(6, kmerMap, testList, 6)

    topSeeds = Seeding.topSeeds(eventArray,numSeeds)

    println("Top seed scores")
    for (i <- 0 until numSeeds) {
      println (Seeding.seedScore(topSeeds(i)) + " Index: " + topSeeds(i).index)
    }

    println("First 100 seed scores")
    for (i <- 0 to 100) {
      println (Seeding.seedScore(eventArray(i)))
    }

  }

}
