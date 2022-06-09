import scala.::
import scala.io.{BufferedSource, Source}

package object helper {

  //deprecated
  def fileToMap (filename: String) : Map[String, Double] = {
    val kmerModelFile: BufferedSource = Source.fromFile(filename)
    var kmerSignalMap: Map[String,Double] = Map()
    for (line <- kmerModelFile.getLines().drop(1)) {
      val data: Array[String] = line.split("\\s{1,}")
      kmerSignalMap += (data(0) -> data(1).toDouble)
    }
    kmerSignalMap
  }

  //refactor of fileToMap with Z-normalization
  //reads kmerModel file, and creates the corresponding kmer -> signal map
  def fileToMapZ (filename:String): Map[String,Double] = {
    val kmerModelFile: BufferedSource = Source.fromFile(filename)
    var kmerSignalMap: Map[String,Double] = Map()

    var sum: Double = 0.0

    for (line <- kmerModelFile.getLines().drop(1)) {
      val data: Array[String] = line.split("\\s{1,}")
      kmerSignalMap += (data(0) -> data(1).toDouble)
      sum += data(1).toDouble
    }

    val denom: Int = kmerSignalMap.size
    val mean: Double = sum/denom

    //determine the standard deviation
    //first determine the variance, i.e. the sum of the squared differences from the mean

    var varianceSum: Double = 0.0
    for (key <- kmerSignalMap.keys) {
      varianceSum += Math.pow(kmerSignalMap(key) - mean,2)
    }
    val variance: Double = varianceSum/denom
    val stdDev: Double = Math.pow(variance,.5)

    for (key <- kmerSignalMap.keys) {
      kmerSignalMap += (key -> ((kmerSignalMap(key) - mean)/stdDev))
    }
    kmerSignalMap
  }

  //Reads a file and converts it into an array[String] representing the sequence
  //deprecated
  def readBases(filename: String, length: Int): Array[String] = {
    val returnArray: Array[String] = new Array[String](length)
    val file: BufferedSource = Source.fromFile(filename)
    var i: Int = 0
    for (line <- file.getLines()) {
      returnArray(i) = line
      i += 1
    }
    returnArray
  }

  //refactored version of readBases, but returns a list instead of an Array
  def readBasesList(filename: String): List[Char] = {
    var returnList: List[Char] = List()
    val file: BufferedSource = Source.fromFile(filename)
    for (line <- file.getLines()) {
      for (i <- 0 until line.length) {
        var base: Char = 255
        val character: Char = line(i).toUpper
        if ((character == 'A') || (character == 'T') || (character == 'C') || (character == 'G')) {
          base = character
        }
        if (base != 255) {
          returnList = base :: returnList
        }
      }
    }
   returnList.reverse
  }

}
