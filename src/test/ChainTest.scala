package test

import helper.{fileToMapZ, readBasesList}
import index.EventSeqVector.listToVector
import index.{KDTree, KDTreeNode, Vector}
import org.scalatest._
import seed.Seeding
import chain._
import preprocess._


class ChainTest extends FunSuite {

  val clip: String = "data/Euglena clip.txt"
  //val fast5read1: String = "data/00.txt"
  val kmerModel: String = "data/6merModel.txt"
  val referenceGenome: String = "data/Euglena gracilis half.fna"

  //Make the kmer-signal map
  val kmerMap: Map[String, Double] = fileToMapZ(kmerModel)
  println ("Made map")

  //Read the bases from reference and clip
  val referenceBases: List[Char] = readBasesList(referenceGenome)
  val clipBases: List[Char] = readBasesList(clip)

  //Convert into an array of vectors
  val referenceVectors: Array[Vector] = listToVector(6,kmerMap,referenceBases,6)
  //val fast5read1Vectors: Array[Vector] = Fast5Process.fast5ToVector(6,kmerMap,fast5read1,6)
  val clipVectors: Array[Vector] = listToVector(6,kmerMap,clipBases,6)

  //Make the kdtree of the reference genome
  val kdTree: KDTreeNode[Vector] = KDTree.makeKDTree(referenceVectors)

  println ("Made kdtree of reference")

  //Find the top seeds
  val numSeeds: Int = 2000
  //val fast5TopSeeds: Array[Vector] = Seeding.topSeeds(fast5read1Vectors,numSeeds)
  val clipTopSeeds: Array[Vector] = Seeding.topSeeds(clipVectors,numSeeds)

  println ("Found top seeds")

  //More constants
  val numAnchors: Int = 500
  val range: Double = 2

  //Find the top anchors

  //val fast5TopAnchors: Array[Anchor] = Chaining.findTopAnchors(referenceVectors, kdTree, fast5TopSeeds, numAnchors, 6, range)
  val clipTopAnchors: Array[Anchor] = Chaining.findTopAnchors(referenceVectors, kdTree, clipTopSeeds, numAnchors, 6, range)

  println ("Found top anchors")

  //val fast5MaxScore: Double = Chaining.maxChainingScore(fast5TopAnchors, range, 6)
  val clipMaxScore: Double = Chaining.maxChainingScore(clipTopAnchors, range, 6)

  test ("Max chain score") {}


    println("Clip of genome max chaining score")
    println(clipMaxScore)

/*
  //Do the same for file 00

  val fast5read1Vectors: Array[Vector] = Fast5Process.fast5ToVector(6,kmerMap,"data/00.txt",6)
  val fast5TopSeeds: Array[Vector] = Seeding.topSeeds(fast5read1Vectors,numSeeds)
  val fast5TopAnchors: Array[Anchor] = Chaining.findTopAnchors(referenceVectors, kdTree, fast5TopSeeds, numAnchors, 6, range)
  val fast5MaxScore: Double = Chaining.maxChainingScore(fast5TopAnchors, range, 6)

  println ("Real read max chaining score")
  println (fast5MaxScore)
*/

  val fastReads: List[String] = List("00", "01", "02", "03", "04", "05", "06", "07", "08", "09")
  for (fileNo <- fastReads) {
    val filename = "data/" + fileNo + ".txt"

    //convert signals into array of vectors
    val f5Vectors: Array[Vector] = Fast5Process.fast5ToVector(6,kmerMap,filename,6)

    //find the tops seeds
    val f5TopSeeds: Array[Vector] = Seeding.topSeeds(f5Vectors,numSeeds)

    //find the top anchors
    val f5TopAnchors: Array[Anchor] = Chaining.findTopAnchors(referenceVectors, kdTree, f5TopSeeds, numAnchors, 6, range)

    //find the max chain score
    val f5MaxScore: Double = Chaining.maxChainingScore(f5TopAnchors, range, 6)
    println ("Max Chain score for read " + fileNo)
    println (f5MaxScore)
  }

}
