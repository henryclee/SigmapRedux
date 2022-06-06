package test

import org.scalatest._
import scala.io.{BufferedSource, Source}
import index.EventSeqVector._
import index.Vector
import index.KDTreeNode
import index.KDTree
import helper._


class IndexTest extends FunSuite {

  //Creates KmerMap with arbitrary values
  def InitKmerMap(): Map[String,Double] = {
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
    kmerMap
  }

  //Reads a file and converts it into an array[String] representing the sequence
  def ReadBases(filename: String, length: Int):Array[String] = {
    var returnArray: Array[String] = new Array[String](length)
    val file: BufferedSource = Source.fromFile(filename)
    var i: Int = 0
    for (line <- file.getLines()){
      returnArray(i) = line
      i += 1
    }
    returnArray

  }

  test("Indexing") {

    val length: Int = 10
    val basesFile: String = "data/RandomBases.txt"
    val kmerModel: String = "data/6merModel.txt"

    //val kmerMap: Map[String,Double] = InitKmerMap()
    val kmerMap: Map[String, Double] = fileToMap(kmerModel)

    val nuclSequences: Array[String] = ReadBases(basesFile,length)

    val vectorSet1: Array[Vector] = seqToVector(6,kmerMap,nuclSequences(0),10)

    //println (kmerMap("CATAGA"))
    //println (kmerMap("ATAGCA"))
    //vectorSet1(0).returnValue

    //Generate KD Tree
    val kdTree: KDTreeNode[Vector] = KDTree.makeKDTree(vectorSet1)

  }

}
