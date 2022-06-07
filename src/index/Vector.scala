package index

class Vector (var tupleValue: Array[Double]){

  def returnValue(): Unit = {
    for (i <- 0 until this.tupleValue.length) {
      println (this.tupleValue(i))
    }
  }

  def dimension(): Int = {
    this.tupleValue.length
  }

  def getDimValue(dimension: Int): Double = {
    this.tupleValue(dimension)
  }

}
