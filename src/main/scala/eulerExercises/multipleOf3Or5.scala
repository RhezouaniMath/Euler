package eulerExercises

import scala.annotation.tailrec

object multipleOf3Or5 {
  @tailrec
  def sumOfNumbers(lower: Int, upper: Int, acc: Int): Int = {
    if (lower <= upper){
      if (lower % 3 == 0 || lower % 5 == 0){
        sumOfNumbers(lower+1, upper, acc+lower)
      }
      else{
        sumOfNumbers(lower+1, upper, acc)
      }
    }
    else{
      acc
    }
  }
}
