package helper

object RandomRawSignal {

  def RandomSignal(): Array[Double] = {

    val r = scala.util.Random
    val length: Int = (r.nextFloat * 100).toInt + 100
    val randomArray: Array[Double] = (for(i <- 1 to length) yield {(r.nextFloat * 1024).toInt.toDouble}).toArray

    randomArray

  }



}
