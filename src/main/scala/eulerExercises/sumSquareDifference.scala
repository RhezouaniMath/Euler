package eulerExercises

object sumSquareDifference {

  def sumOfSquares(nr: Int): Long={
    val seq = for (i<- 1 to nr ) yield i
    val longSeq = seq map(_.toLong)
    val squaresSeq = longSeq map(x => x*x)
    squaresSeq.sum
  }

  def squareOfSum(nr: Int): Long={
    val seq = for (i<- 1 to nr ) yield i
    val longSeq = seq map(_.toLong)
    val sum = longSeq.sum
    sum*sum
  }

  def difference(nr: Int): Long={
    squareOfSum(nr) - sumOfSquares(nr)
  }

  def firstNdifferences(N: Int): List[Long]={
    val seq = for (i<- 1 to N) yield i
    val list = seq.toList
    list map(x=> difference(x))
  }
}
