package index

//Vector class, state variable tupleValue holds a tuple. The length of the tuple is defined upon creation
class Vector (var tupleValue: Array[Double]){

  //Returns the tupleValue --  may not be necessary?
  def returnValue(): Unit = {
    for (i <- 0 until this.tupleValue.length) {
      println (this.tupleValue(i))
    }
  }

  //May not be necessary
  def dimension(): Int = {
    this.tupleValue.length
  }

  //May not be necessary
  def getDimValue(dimension: Int): Double = {
    this.tupleValue(dimension)
  }

}
