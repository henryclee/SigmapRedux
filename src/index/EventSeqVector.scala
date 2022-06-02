package index

class EventSeqVector (val k: Int, val kmerMap: Map [String,Double], sequence: Array[Char]) {

  def toVectors (dim: Int): Array[Array[Double]] = {

    //Number of kmers in the sequence
    val kmerNo: Int = sequence.length - k + 1
    val vectorNo: Int = sequence.length - k - dim + 2

    var kmerSeq: Array[String] = new Array[String](kmerNo)
    var eventSeq: Array[Double] = new Array[Double](kmerNo)
    var vectorSet: Array[Array[Double]] = new Array[Array[Double]](vectorNo)

    //This loop populates eventSeq with the events from the sequence
    for (i <- 0 until kmerNo) {
      var kmer: String = ""
      for (j <- 0 until k) {
        kmer += sequence(i+j)
      }
      eventSeq(i) = kmerMap(kmer)
    }

    //This loop populates the vectorSet with events as tuples
    for (i <- 0 until vectorNo) {
      for (j <- 0 until dim) {
        vectorSet(i)(j) = eventSeq(i+j)
      }
    }
    vectorSet
  }


}
