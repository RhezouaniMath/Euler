package eulerExercises

import scala.annotation.tailrec

object fibonacci {

  def nextFibonacciNr(first: Int, second: Int): Int = {first + second}

  @tailrec
  def listOfFibonacciNrs(initial: List[Int], upperBound: Int): List[Int] = initial match {
    case Nil => throw new IllegalArgumentException
    case second::Nil => throw new IllegalArgumentException
    case second::first::xs =>
      if (second < upperBound){
        val nextNr = nextFibonacciNr(first, second)
        val newInitial = nextNr::initial
        listOfFibonacciNrs(newInitial, upperBound)
      }
      else{
        initial
      }
  }

  def sumOfEvenNumbered(upperBound: Int): Int ={
    val listOfNrs = listOfFibonacciNrs(List(2,1), upperBound)
    val filteredList = listOfNrs filter(x => x%2 == 0)
    filteredList.sum
  }

}
