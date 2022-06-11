package preprocess

import index.Vector

import scala.io.{BufferedSource, Source}

object Fast5Process {

  //read a fast5 file of raw signals, and return it as a list that is Z normalized
  def f5ToListZ (filename:String) : List[Double] = {

    var signalList: List[Double] = List()

    var sum: Double = 0.0
    var denom: Int = 0

    //determine the mean

    var file: BufferedSource = Source.fromFile(filename)
    for (line <- file.getLines()) {
      sum += line.toDouble
      denom += 1
    }

    val mean: Double = sum/denom

    //determine the variance and standard deviation

    var varianceSum: Double = 0

    file = Source.fromFile(filename)
    for (line <- file.getLines()) {
      varianceSum += Math.pow(line.toDouble - mean,2)
    }

    val variance: Double = varianceSum/denom
    val stdDev: Double = Math.pow(variance,.5)

    //create the Z-normalized signal list

    file = Source.fromFile(filename)
    for (line <- file.getLines()) {
      signalList = (line.toDouble - mean)/stdDev :: signalList
    }
    signalList.reverse
  }

  //takes a fast5 file, and converts it into an array of vectors
  def fast5ToVector (k: Int, kmerMap: Map [String,Double], filename: String, dim: Int): Array[Vector] = {

    //First, Z-normalize the signals
    val eventArray: List[Double] = f5ToListZ(filename)

    //Calculate the total no of kmers and vectors
    val kmerNo: Int = eventArray.length - k + 1
    val vectorNo: Int = eventArray.length - k - dim + 2

    val vectorSeq: Array[Vector] = new Array[Vector](vectorNo)

    //This loop populates the vectorSeq with events as tuples
    for (i <- 0 until vectorNo) {
      val tempArray: Array[Double] = new Array[Double](dim)
      for (j <- 0 until dim) {
        tempArray(j) = eventArray(i+j)
      }
      vectorSeq(i) = new Vector(i, tempArray)
    }
    vectorSeq
  }

}
