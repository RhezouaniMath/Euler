package eulerExercises

import scala.annotation.tailrec

object NthPrime {

  def NthPrime(N: Long): Long ={

    @tailrec
    def helpFunction(count: Long, nr: Long, N: Long): Long={
      if(LargestPrimeFactor.isPrime(nr)){
        if(count + 1 == N) {
          nr
        }
        else{
          helpFunction(count+1,nr+1,N)
        }
      }
      else{
        helpFunction(count, nr+1,N)
      }
    }

    helpFunction(0L,1L,N)

  }

}
