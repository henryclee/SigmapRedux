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
}
