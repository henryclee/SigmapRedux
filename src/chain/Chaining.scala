package chain

import index._
import helper._
import index.EventSeqVector._

object Chaining {

  //Find the top numAnchors anchors, by smallest normSq between the vectors
  def findTopAnchors(reference: Array[Vector], refTree: KDTreeNode[Vector], input: Array[Vector], numAnchors: Int, dimensions: Int, range: Double):Array[Anchor] = {

    var topAnchors: Array[Anchor] = new Array[Anchor](numAnchors)
    val defaultAnchor: Anchor = new Anchor(0,0,9999)

    //initialize topAnchors
    for (i <- 0 until numAnchors) {
      topAnchors(i) = defaultAnchor
    }

    //Ranks the anchors based on the value of of the distance between the corresponding vectors
    val comparator: (Anchor, Anchor) => Boolean = (a: Anchor, b: Anchor) => {
      a.normSq < b.normSq
  //    (normSq(reference(a.refIndex), input(a.readIndex)) < normSq(reference(b.refIndex),input(b.readIndex)))
    }

    //Iterate through all of the input vectors, find the nearest neighbors on the kd tree within range, create anchors for each
    //pair of vectors, and find the top numAnchors, sorted by lowest distance

    for (i <- 0 until numAnchors) {
      //First, let's find the nearest neighbors to the input vector on the kdtree
      var nearestNeighbors: List[Vector] = KDTree.findNNR(refTree, input(i), dimensions, range)
      for (j <- nearestNeighbors) {
        //create an anchor between the nearest neighbor and the input, and put it into topAnchors if it's in the top numAnchors
        val tempAnchor: Anchor = new Anchor(i, j.index, normSq(j, input(i)))
        if (tempAnchor.normSq < topAnchors(numAnchors-1).normSq) {
          topAnchors(numAnchors-1) = tempAnchor
          topAnchors = topAnchors.sortWith(comparator)
        }
      }
    }
    topAnchors
  }

  //Finds the maximum chaining score for a set of anchors
  def maxChainingScore(unsortedAnchors: Array[Anchor], range: Double, dimension: Int ): Double = {

    //Constants
    val maxGap: Double = 100
    val maxGapScale: Double = 4

    //Sort the anchors by reference index
    val anchors = unsortedAnchors.sortBy(_.refIndex)

    //Create a matrix for seedHits and gapPenalty
    var seedHit: Array[Array[Double]] = Array.ofDim[Double](anchors.length, anchors.length)
    var gapPenalty: Array[Array[Double]]= Array.ofDim[Double](anchors.length, anchors.length)

    //Create an array of best chaining scores
    var chainScore: Array[Double] = new Array[Double](anchors.length)

    //Base case
    chainScore(0) = (1 - anchors(0).normSq)/dimension

    var maxChainScore: Double = 0.0
    var tempScore: Double = 0.0

    //Initialize the matrix for seedHit and gapPenalty
    //These formulas are from the sigmap paper
    for (i <- 1 until anchors.length) {
      for (j <- 0 until i) {
        seedHit(j)(i) = (1- anchors(i).normSq/range) * Math.min(Math.min(anchors(i).refIndex - anchors(j).refIndex, anchors(i).readIndex - anchors(j).readIndex), dimension)
        if ((anchors(i).readIndex < anchors(j).readIndex) ||
          (Math.abs((anchors(i).refIndex - anchors(j).refIndex) - (anchors(i).readIndex - anchors(j).readIndex)) > maxGap) ||
          (Math.abs((anchors(i).refIndex - anchors(j).refIndex) / Math.max(anchors(i).readIndex - anchors(j).readIndex,0.1)) > maxGapScale)) {

          gapPenalty(j)(i) = 9999
        }
        else {
          gapPenalty(j)(i) = 0
        }
      }
    }

    //Now determine the chain scores
    for (i <- 1 until anchors.length) {
      for (j <- 0 until i) {
        tempScore = Math.max(chainScore(j) + seedHit(j)(i) - gapPenalty(j)(i), (1 - anchors(i).normSq)/dimension)
        chainScore(i) = Math.max(chainScore(i),tempScore)
      }
      maxChainScore = Math.max(maxChainScore,chainScore(i))
    }
    maxChainScore
  }

}
