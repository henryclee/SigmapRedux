package index

class Vector (var value: Array[Double]){

  def returnValue(): Unit = {
    for (i <- 0 until this.value.length) {
      println (this.value(i))
    }
  }

  def dimension(): Int = {
    this.value.length
  }

  def getDimValue(dimension: Int): Double = {
    this.value(dimension)
  }

}
