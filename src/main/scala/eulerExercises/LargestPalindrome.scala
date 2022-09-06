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
      if (lengthFactors == 0){ acc }
      else{
        val list = for(string <- acc; i<- 1 to 9) yield string + i
        helpFactor(lengthFactors -1, list)
      }
    }

    @tailrec
    def minimumValueFunction(lengthFactors: Int, acc: Int): Int ={
      if (lengthFactors == 0){ acc }
      else{
        minimumValueFunction(lengthFactors-1, 10*acc)
      }
    }

    val minimumValue = minimumValueFunction(lengthFactors-1,1)
    val stringList = helpFactor(lengthFactors, List(""))
    val intList = stringList map(_.toInt)
    val filteredList = intList filter(x => x>=minimumValue)
    val products = for (factor1 <- filteredList; factor2 <- filteredList) yield factor1 * factor2
    val palindromeProducts = products filter(x=> isPalindrome(x))
    val largest = palindromeProducts.max
    largest
  }

}
