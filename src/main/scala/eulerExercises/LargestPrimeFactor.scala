package eulerExercises
import scala.annotation.tailrec
import scala.math._

object LargestPrimeFactor {

  def isPrime(nr: Long): Boolean={

    @tailrec
    def isPrimeHelp(lower: Long, upper: Long, nr: Long): Boolean = {
      if (lower > upper){
        true
      }
      else{
        if (nr%lower == 0){ false }
        else { isPrimeHelp(lower+1, upper, nr)}
      }
    }

    if (nr < 2){
      false
    }
    else{

      val doubleNr = nr.toDouble
      val sqrtNr = sqrt(doubleNr)
      val floorSqrt = floor(sqrtNr).toLong

      isPrimeHelp(2, floorSqrt, nr)

    }
  }

  def largestPrimeFactor(nr: Long): Long ={
    @tailrec
    def largestPrimeFactorHelp(nr: Long, acc:Long): Long ={
      if ( nr%acc == 0 && isPrime(acc)){ acc }
      else {largestPrimeFactorHelp(nr, acc-1) }
    }
    largestPrimeFactorHelp(nr, nr)
  }
}
