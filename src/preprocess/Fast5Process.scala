package preprocess

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

}
