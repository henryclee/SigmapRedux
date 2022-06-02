package test

import org.scalatest._

class Index extends FunSuite {

  test("Indexing") {

    var kmerMap: Map[String, Double] = Map()
    val nucleotide: Array [String] = Array ("A","T","C","G")

    // expected current for the k-mer-expected current mapping is defined arbitrarily between 0-1023
    var current: Double = 0.0

    //populating the k-mer-expected event for a 5-mer
    for (i <- 0 to 3) {
      for (j <- 0 to 3) {
        for (k <- 0 to 3) {
          for (l <- 0 to 3) {
            for (m <- 0 to 3) {
              val kmer: String = nucleotide(i) + nucleotide(j) + nucleotide(k) + nucleotide(l) + nucleotide(m)
              kmerMap = kmerMap + (kmer -> current)
              current += 1.0
            }
          }
        }
      }
    }





  }

}
