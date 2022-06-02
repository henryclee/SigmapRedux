package index

import scala.annotation.tailrec

object KDTree {

  @tailrec
  def populateTree(head: KDTreeNode[Vector], child: KDTreeNode[Vector], dimensions: Int, dimPointer: Int): Unit = {

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
        populateTree (head.left, child, dimensions, newPointer)
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
        populateTree(head.right, child, dimensions, newPointer)
      }
    }
  }

  //NOT Balanced, just filling
  def makeKDTree(vectors: Array[Vector]) : KDTreeNode[Vector] = {

    val head: KDTreeNode[Vector] = new KDTreeNode (vectors(0), null, null)

    for (i <- 1 until vectors.length) {
      val child: KDTreeNode[Vector] = new KDTreeNode (vectors(i), null, null)
      populateTree(head,child,vectors(0).dimension(),0)
    }
    head
  }
}
