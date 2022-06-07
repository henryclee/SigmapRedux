package index

import scala.annotation.tailrec

object KDTree {

  @tailrec
  def populateTreeHelper(head: KDTreeNode[Vector], child: KDTreeNode[Vector], dimensions: Int, dimPointer: Int): Unit = {

    //check if child's value at dimension is less than parent's value at dimension (which side of hyperplane)
    if (child.value.getDimValue(dimPointer) <= head.value.getDimValue(dimPointer)) {
      //base case - place child in left branch
      if (head.left == null) {
        head.left = child
      }
      //need to recurse down left branch, and increment dimPointer
      else {
        //increment dimPointer, or reset to 0
        var newPointer: Int = dimPointer + 1
        if (newPointer == dimensions) {
          newPointer = 0
        }
        populateTreeHelper (head.left, child, dimensions, newPointer)
      }
    }
    //child goes to right
    else {
      //base case
      if (head.right == null) {
        head.right = child
      }
      else {
        //increment dimPointer, or reset to 0
        var newPointer: Int = dimPointer + 1
        if (newPointer == dimensions) {
          newPointer = 0
        }
        populateTreeHelper(head.right, child, dimensions, newPointer)
      }
    }
  }

  //Makes a KD tree from an array of vector, returning root of the kdtree
  //NOT Balanced, just filling
  def makeKDTree(vectors: Array[Vector]) : KDTreeNode[Vector] = {

    val head: KDTreeNode[Vector] = new KDTreeNode (vectors(0), null, null)

    for (i <- 1 until vectors.length) {
      val child: KDTreeNode[Vector] = new KDTreeNode (vectors(i), null, null)
      populateTreeHelper(head,child,vectors(0).dimension(),0)
    }
    head
  }

  //Given 2 vectors, returns the square of the norm squared, to avoid having to do a sqrt
  def normSq (v1: Vector, v2: Vector) : Double = {
    var norm2: Double = 0.0
    for (i <- v1.tupleValue.indices) {
      norm2 += Math.pow(v1.tupleValue(i) - v2.tupleValue(i),2)
    }
    norm2
  }

  def findNNHelper(parent: KDTreeNode[Vector], input: Vector, dimensions: Int, dimPointer: Int, currentBest: Vector) : Vector = {

    //update newPointer
    var newPointer: Int = dimPointer + 1
    if (newPointer == dimensions) {
      newPointer = 0
    }
    var newBest: Vector = currentBest
    var currentBestNormSq: Double = normSq(input, currentBest)

    var parentInputDist: Double = normSq(input, parent.value)
    if (parentInputDist < currentBestNormSq) {
      newBest = parent.value
    }

    //base case, parent is a leaf node
    if (parent.left == null && parent.right == null) {
      newBest
    }
    //check if input's value at dimension is less than parent's value at dimension (which side of hyperplane do we go to)
    //if input < parent, go left
    else if (input.tupleValue(dimPointer) < parent.value.tupleValue(dimPointer)) {
      //if the left node is null, can't go left anymore
      if (parent.left == null) {
        //check if the hypersphere around parent.value vector is larger than the distance to the hyperplane
        //if so, need to check the right path
        if (normSq(input, newBest) > Math.pow(parent.value.tupleValue(dimPointer) - newBest.tupleValue(dimPointer),2)){
          newBest = findNNHelper(parent.right, input, dimensions, newPointer, newBest)
        }
      }
      // if left is a valid node, we go recursively to the left
      else {
        newBest = findNNHelper(parent.left, input, dimensions, newPointer, newBest)
      }
    }
    //if we don't go left, we go right
    else {
      //first check if right null
      if (parent.right == null) {
        //check hypersphere again to see if we're done
        if (normSq(input, newBest) > Math.pow(parent.value.tupleValue(dimPointer) - newBest.tupleValue(dimPointer), 2)) {
          newBest = findNNHelper(parent.left, input, dimensions, newPointer, newBest)
        }
      }
      // if right is a valid node, go recursively to the right
      else {
        newBest = findNNHelper(parent.right, input, dimensions, newPointer, newBest)
      }
    }
    newBest
  }

  //Given the root of a kd tree, and an arbitrary vector, find the nearest neighbor in the tree
  def findNearestNeighbor (kdTree: KDTreeNode[Vector], input: Vector, dimensions: Int) : Vector = {

    val nearestNeighbor: Vector = findNNHelper(kdTree, input, dimensions, 0, kdTree.value)
    nearestNeighbor

  }


}
