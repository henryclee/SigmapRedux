package test

import org.scalatest._

import index.EventSeqVector._
import index.Vector
import index.KDTreeNode
import index.KDTree
import helper._
import preprocess.Fast5Process

class KDTreeTest extends FunSuite {

  val clip: String = "data/Euglena clip.txt"
  val kmerModel: String = "data/6merModel.txt"
  val referenceGenome: String = "data/Euglena gracilis half.fna"

  //Make the kmer-signal map
  val kmerMap: Map[String, Double] = fileToMapZ(kmerModel)
  println ("Made map")

  //Read the bases from reference, fast5, and random
  val referenceBases: List[Char] = readBasesList(referenceGenome)
  val randomBases: List[Char] = readBasesList(clip)

  println ("Read bases files")

  //Convert into an array of vectors
  val referenceVectors: Array[Vector] = listToVector(6,kmerMap,referenceBases,6)
  val randomVectors: Array[Vector] = listToVector(6,kmerMap,randomBases,6)

  println("Created arrays of vectors")

  //Make the kdtree of the reference genome
  val kdTree: KDTreeNode[Vector] = KDTree.makeKDTree(referenceVectors)

  println ("Made kdtree of reference")

  println ("Nearest neighbor test")
  for (i <- 11 to 20) {
    val NN: Vector = KDTree.findNN (kdTree, referenceVectors(i), 6)
    println("Nearest neighbors to input " + i)
    println (normSq(NN, referenceVectors(i)))
  }

  println ("Nearest neighbor range test")
  for (i <- 11 to 20) {
    val NNR: List[Vector] = KDTree.findNNR(kdTree, referenceVectors(i),6,.1)
    println ("Nearest neighbors to input " + i)
    for (j <- NNR.indices) {
      println (normSq(NNR(j), referenceVectors(i)))
    }
  }



}
