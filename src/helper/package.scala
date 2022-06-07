import scala.io.{BufferedSource,Source}

package object helper {

  def fileToMap (filename: String) : Map[String, Double] = {
    val kmerModelFile: BufferedSource = Source.fromFile(filename)
    var kmerSignalMap: Map[String,Double] = Map()
    for (line <- kmerModelFile.getLines().drop(1)) {
      val data: Array[String] = line.split("\\s{1,}")
      kmerSignalMap += (data(0) -> data(1).toDouble)
    }
    kmerSignalMap
  }

  //Reads a file and converts it into an array[String] representing the sequence
  def readBases(filename: String, length: Int): Array[String] = {
    var returnArray: Array[String] = new Array[String](length)
    val file: BufferedSource = Source.fromFile(filename)
    var i: Int = 0
    for (line <- file.getLines()) {
      returnArray(i) = line
      i += 1
    }
    returnArray
  }
}
