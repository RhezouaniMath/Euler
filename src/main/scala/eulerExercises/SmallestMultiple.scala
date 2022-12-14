package eulerExercises

import scala.annotation.tailrec

object SmallestMultiple {

  def isMultiple(multiple: Long, nr: Int): Boolean={
    val seq = for (i<- 1 to nr) yield multiple%i
    val sum = seq.sum
    sum == 0
  }

  @tailrec
  def reduce(multiple: Long, count: Int, nr: Int): Long={
    if (count > 1){
      if(multiple%count == 0 && isMultiple(multiple/count, nr) ){
        reduce(multiple/count, count, nr)
      }
      else{
        reduce(multiple, count-1, nr)
      }
    }
    else{
      multiple
    }
  }

  def smallestMultiple(nr: Int): Long={
    val list = (1 to nr).toList
    val longList = list map (_.toLong)
    val prod = longList.product
    reduce(prod, nr, nr)
  }

}
