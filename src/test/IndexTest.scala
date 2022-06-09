package test

import org.scalatest._
import scala.io.{BufferedSource, Source}
import index.EventSeqVector._
import index.Vector
import index.KDTreeNode
import index.KDTree
import helper._


class IndexTest extends FunSuite {

  val kmerModel: String = "data/6merModel.txt"
  val kmerMap: Map[String, Double] = fileToMapZ(kmerModel)

  test("Indexing") {

    val basesFile: String = "data/RandomBases.txt"
    val length: Int = 10

    val nuclSequences: Array[String] = readBases(basesFile,length)
    val nuclSeqList: List[Char] = readBasesList(basesFile)

    val vectorSet1: Array[Vector] = seqToVector(6,kmerMap,nuclSequences(0),10)

    //println (kmerMap("CATAGA"))
    //println (kmerMap("ATAGCA"))
    //vectorSet1(0).returnValue

    //Generate KD Tree
    val kdTree: KDTreeNode[Vector] = KDTree.makeKDTree(vectorSet1)

  }

  test("kdTree") {

    val vector1: Vector = new Vector(Array(6,6))
    val vector2: Vector = new Vector(Array(4,4))
    val vector3: Vector = new Vector(Array(10,7))
    val vector4: Vector = new Vector(Array(2,8))
    val vector5: Vector = new Vector(Array(8,10))
    val vector6: Vector = new Vector(Array(9,2))
    val test1: Vector = new Vector(Array(5,3)) //nearest to vector2
    val test2: Vector = new Vector(Array(7,5)) //nearest to vector1

    val sequence: Array[Vector] = Array(vector1, vector2, vector3, vector4, vector5, vector5, vector6)
    val kdTree: KDTreeNode[Vector] = KDTree.makeKDTree(sequence)

    assert (KDTree.findNearestNeighbor(kdTree,test1,2) == vector2)
    assert (KDTree.findNearestNeighbor(kdTree,test2, 2) == vector1)

  }

  var testList: List[Char] = List()
  var eventArray: Array[Vector] = Array()

  test ("readBaseList") {

    //Read file and return a list of char
    val filename: String = "data/Euglena gracilis half.fna"
    testList = readBasesList(filename)
    println (testList.head)

  }

  test ("Sequence to array of Vectors") {

    eventArray = listToVector(6, kmerMap, testList, 6)
    println("Vector 0")
    eventArray(0).returnValue()
    println("Vector 1")
    eventArray(1).returnValue()
    println("Vector 2")
    eventArray(2).returnValue()
    println("Vector 3")
    eventArray(3).returnValue()

  }




}
