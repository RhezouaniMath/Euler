package eulerExercises

import scala.annotation.tailrec

object LargestPalindrome {

  def isPalindrome(nr: Int): Boolean={
    val stringNr = nr.toString
    stringNr == stringNr.reverse
  }

  def largest(lengthFactors: Int): Int ={

    @tailrec
    def helpFactor(lengthFactors: Int, acc: List[String]): List[String] ={
      if (lengthFactors == 0){
        acc
      }
      else{
        val list = for(string <- acc; i<- 0 to 9) yield string + i
        helpFactor(lengthFactors -1, list)
      }
    }

    val power = lengthFactors - 1
    val minimumValue = 10^power
    val stringList = helpFactor(lengthFactors, List(""))
    val intList = stringList map(_.toInt)
    val filteredList = intList filter(x => x>=minimumValue)
    val products = for (factor1 <- filteredList; factor2 <- filteredList) yield factor1 * factor2
    val palindromeProducts = products filter(x=> isPalindrome(x))
    val largest = palindromeProducts.max
    largest

  }

}



/*
    @tailrec
    def minimumValueFunction(lengthFactors: Int, acc: Int): Int ={
      if (lengthFactors == 0){ acc }
      else{
        minimumValueFunction(lengthFactors-1, 10*acc)
      }
    }
 */