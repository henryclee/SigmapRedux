package seed

import index._

object Seeding {

  //Scoring by highest sum of pairwise difference between signals in the tuple
  def seedScore(seed: Vector):Double = {

    var score: Double = 0.0
    for (i <- 1 until seed.dimension()) {
      score += (seed.tupleValue(i) - seed.tupleValue(i-1)).abs
    }
    score
  }

  def comparator(a: Vector, b: Vector) : Unit = {}

  //Find the top seeds in terms of variance in the signal, from the paper
  def topSeeds(sequence: Array[Vector], numSeeds: Int): Array[Vector] = {

    //initialize the topSeeds Array
    var topSeeds: Array[Vector] = new Array[Vector](numSeeds)
    val comparator: (Vector,Vector) => Boolean = (a: Vector, b: Vector) => {
      seedScore(a) > seedScore(b)
    }

    //First, initialize topSeeds with just the first numSeeds of the sequence
    for (i <- 0 until numSeeds) {
      topSeeds(i) = sequence(i)
    }

    //Next, sort topSeeds from highest to lowest
    topSeeds = topSeeds.sortWith(comparator)

    //Now, check the rest of the array, and insert into top iff it is greater than the last spot
    for (i <- numSeeds until sequence.length) {
      //if the vector score is higher than the smallest of the topSeeds
      //then resort the topSeeds
      if (seedScore(sequence(i)) > seedScore(topSeeds(numSeeds-1))){
        topSeeds(numSeeds-1) = sequence(i)
        topSeeds = topSeeds.sortWith(comparator)
      }
    }

    topSeeds

  }

}
