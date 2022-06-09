package index

object EventSeqVector{

  //Converts a sequence of nucleotide bases into an array of vectors, where each vector represents
  //a tuple of consecutive "events" (expected current, as a double) from each kmer from kmerMap.
  //k is the size of the kmer.
  def seqToVector(k: Int, kmerMap: Map [String,Double], sequence: String, dim: Int): Array[Vector] = {

    //Number of kmers in the sequence
    val kmerNo: Int = sequence.length - k + 1
    val vectorNo: Int = sequence.length - k - dim + 2

    var kmerSeq: Array[String] = new Array[String](kmerNo)
    var eventSeq: Array[Double] = new Array[Double](kmerNo)
    var vectorSet: Array[Vector] = new Array[Vector](vectorNo)

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
      var tempArray: Array[Double] = new Array[Double](dim)
      for (j <- 0 until dim) {
        tempArray(j) = eventSeq(i+j)
      }
      vectorSet(i) = new Vector(tempArray)
    }
    vectorSet
  }

  //seqtoVector refactored using a list of char instead of a string
  //Converts a sequence of nucleotide bases into an array of vectors, where each vector represents
  //a tuple of consecutive "events" (expected current, as a double) from each kmer from kmerMap.
  //k is the size of the kmer.
  def listToVector(k: Int, kmerMap: Map [String,Double], sequence: List[Char], dim: Int): Array[Vector] = {

    var tempSeq = sequence
    //Number of kmers in the sequence
    val kmerNo: Int = sequence.length - k + 1
    val vectorNo: Int = sequence.length - k - dim + 2

    val eventSeq: Array[Double] = new Array[Double](kmerNo)
    val vectorSet: Array[Vector] = new Array[Vector](vectorNo)

    //This loop populates eventSeq with the events from the sequence
    for (i <- 0 until kmerNo) {
      var kmer: String = ""
      for (j <- 0 until k) {
        kmer += tempSeq(j)
      }
      eventSeq(i) = kmerMap(kmer)
      tempSeq = tempSeq.drop(1)
    }

    //This loop populates the vectorSet with events as tuples
    for (i <- 0 until vectorNo) {
      val tempArray: Array[Double] = new Array[Double](dim)
      for (j <- 0 until dim) {
        tempArray(j) = eventSeq(i+j)
      }
      vectorSet(i) = new Vector(tempArray)
    }
    vectorSet
  }

}
