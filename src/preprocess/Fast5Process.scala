package preprocess

import index.Vector

import scala.io.{BufferedSource, Source}

object Fast5Process {

  //read a fast5 file of raw signals, and return it as a list that is Z normalized
  def f5ToListZ (filename:String) : List[Double] = {

    var signalList: List[Double] = List()

    var sum: Double = 0.0
    var denom: Int = 0

    //determine the mean

    var file: BufferedSource = Source.fromFile(filename)
    for (line <- file.getLines()) {
      sum += line.toDouble
      denom += 1
    }

    val mean: Double = sum/denom

    //determine the variance and standard deviation

    var varianceSum: Double = 0

    file = Source.fromFile(filename)
    for (line <- file.getLines()) {
      varianceSum += Math.pow(line.toDouble - mean,2)
    }

    val variance: Double = varianceSum/denom
    val stdDev: Double = Math.pow(variance,.5)

    //create the Z-normalized signal list

    file = Source.fromFile(filename)
    for (line <- file.getLines()) {
      signalList = (line.toDouble - mean)/stdDev :: signalList
    }
    signalList.reverse
  }

  //Helper function to create a list of arrays. Array will hold 2 values: mean, std dev
  //Parameters will be the fast5 signal as a list of doubles, and the window size
  def makeReferenceArray(eventArray: Array[Double], window: Int) : Array[Array[Double]] = {

    val numWindows: Int = eventArray.length - window + 1

    //PRETTY SURE THIS IS WRONG
    var referenceArray: Array[Array[Double]] = Array.ofDim[Double](numWindows,2)

    //Iterate through the list of signals, and find the mean / std dev for each window
    for (i <- 0 until numWindows) {
      var sum: Double = 0.0
      //find mean
      for (j <- i until i + window) {
        sum += eventArray(j)
      }
      val mean: Double = sum / window
      var variance: Double = 0.0
      //find stdev
      for (j <- i until i + window) {
        variance += Math.pow(mean-eventArray(j),2)
      }
      val stddev: Double = Math.pow(variance/window,.5)
      //populate referenceArray
      referenceArray(i) = Array(mean, stddev)
    }
    referenceArray
  }

  //Helper function to create an array of t-values, so we don't need to calculate them twice
  //Parameters: signalArray: Array of Doubles, refArray: Array of Array of doubles, window: Int
  //Returns an array of doubles representing t values for each sliding window of window*2

  def makeTArray(signalArray: Array[Double], refArray: Array[Array[Double]], window: Int): Array[Double] = {

    //Number of t values
    val numT: Int = signalArray.length - (window * 2) + 1
    var tArray: Array[Double] = new Array[Double](numT)

    for (i <- 0 until numT) {
      val index1: Int = i           //index of first window
      val index2: Int = i + window  //index of second window
      //calculate the pooled standard deviation
      val pooledSDsq: Double = ((window-1)*Math.pow(refArray(index1)(1),2) + (window-1)*Math.pow(refArray(index2)(1),2)) / ((window*2)-2)
      val pooledSD: Double = Math.pow(pooledSDsq,.5)
      val tValue: Double = Math.abs((refArray(index1)(0) - refArray(index2)(0)) / (pooledSD * Math.pow(2/window.toDouble,.5)))
      println ("T Value: " + tValue)
      tArray(i) = tValue
    }
    tArray
  }

  //Helper function to determine whether there is a boundary at point b
  //Parameters will be int b as the potential boundary, the reference array refArray holding the mean/sd, tArray holding t values to
  //minimize repetitive calculations, the window size window, threshold value threshold
  //and peak height value ph
  def isBoundary (b: Int, tArray: Array[Double], window: Int, threshold: Double, ph: Double ): Boolean = {

    var answer: Boolean = false

    //We need to convert the boundary index value into the respective index values for refArray, as well as the index value for tArray
    val tArrayIndex: Int = b - window

    //first make sure we're not looking at the first or last windows, since they can't be local peaks
    if (!(b <= window) && !(b>=tArray.length-1)){
      //next check to see if this is local peak
      if ((Math.abs(tArray(tArrayIndex-1) - tArray(tArrayIndex)) >= ph) && (Math.abs(tArray(tArrayIndex+1) - tArray(tArrayIndex)) >= ph)) {
        //finally, check if the t value is greater than the threshold
        if (Math.abs(tArray(tArrayIndex)) >= threshold) {
          answer = true
        }
      }
    }
    answer
  }

  // Helper to create a new list of doubles representing the signal array after segmentation using the boundaries
  def makeEventList (signalArray: Array[Double], boundaryList: List[Int]): List[Double] = {

    var tempList = boundaryList
    var eventList: List[Double] = List()

    var sum: Double = 0

    //first element
    for (i <- 0 until tempList.head) {
      sum += signalArray(i)
    }
    eventList = List(sum/ tempList.head)

    //middle elements
    while (tempList.size > 1) {
      sum = 0
      for (i <- tempList.head until tempList(1)) {
        sum += signalArray(i)
      }
      eventList = sum/(tempList(1) - tempList.head) :: eventList
      tempList = tempList.drop(1)
    }

    //last element
    sum = 0
    for (i <- tempList.head until signalArray.length) {
      sum += signalArray(i)
    }
    eventList = sum/(signalArray.length - tempList.head) :: eventList

    eventList
  }


  //takes a fast5 file, and converts it into an array of vectors
  def fast5ToVector (k: Int, kmerMap: Map [String,Double], filename: String, dim: Int): Array[Vector] = {

    //First, Z-normalize the signals
    val signalList: List[Double] = f5ToListZ(filename)

    //I think converting to an array makes sense, since we will be accessing it by index
    val signalArray: Array[Double] = signalList.toArray

    //Parameters
    val window1: Int = 5
    val window2: Int = 4
    val threshold1: Double = 2.5
    val threshold2: Double = 3.3
    val peakHeight: Double = .2

    //Populate reference arrays
    val w1RefArray: Array[Array[Double]] = makeReferenceArray(signalArray,window1)
    val w2RefArray: Array[Array[Double]] = makeReferenceArray(signalArray,window2)

    //Populate t arrays
    val w1TArray: Array[Double] = makeTArray(signalArray,w1RefArray,window1)
    val w2TArray: Array[Double] = makeTArray(signalArray,w2RefArray,window2)

    //Create a list of boundaries
    var boundaryList: List[Int] = List()

    //Populate boundaryList with boundaries
    //We will define the boundary as the index of the first element of window2
    for (b <- Math.min(window1,window2)  to (signalArray.length - Math.min(window1, window2))) {
      if (isBoundary(b, w1TArray, window1, threshold1, peakHeight) || isBoundary(b, w2TArray, window2, threshold2, peakHeight)) {
        boundaryList = b :: boundaryList
      }
    }
    boundaryList = boundaryList.reverse

    println ("Boundary list length: " + boundaryList.length)

    val eventList: List[Double] = makeEventList(signalArray, boundaryList)

    //Calculate the total no of kmers and vectors
    //val kmerNo: Int = eventList.length - k + 1
    val vectorNo: Int = eventList.length - k - dim + 2

    val vectorSeq: Array[Vector] = new Array[Vector](vectorNo)

    //This loop populates the vectorSeq with events as tuples
    for (i <- 0 until vectorNo) {
      val tempArray: Array[Double] = new Array[Double](dim)
      for (j <- 0 until dim) {
        tempArray(j) = eventList(i+j)
      }
      vectorSeq(i) = new Vector(i, tempArray)
    }
    vectorSeq

  }

}
