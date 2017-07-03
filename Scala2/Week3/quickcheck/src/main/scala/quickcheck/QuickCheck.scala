package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for (
      number <- arbitrary[Int];
      aHeap <- oneOf(const(empty), genHeap)
    ) yield insert(number, aHeap)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insertingTwoNumbersShouldReturnSmaller") = forAll { (numberOne : Int, numberTwo : Int) =>
    var theHeap = insert(numberOne, empty)
    theHeap = insert(numberTwo, theHeap)

    val smallerNumber = if (numberOne <= numberTwo) numberOne else numberTwo

    findMin(theHeap) == smallerNumber
  }

  property("insertingThenRemovingShouldResultInEmpty") = forAll { (theNumber : Int) =>
    var theHeap : H = insert(theNumber, empty)
    theHeap = deleteMin(theHeap)

    isEmpty(theHeap)
  }

  property("removingResultSorted") = forAll { (h: H) =>

    var heap = h

    val numbers = getElementsInOrder(heap)

    numbers.sorted == numbers
  }

  property("minimumOfMeldingOfTwoHeaps") = forAll { (heapOne : H, heapTwo : H) =>

    val meldHeap : H = meld(heapOne, heapTwo)

    val minOfMeld = findMin(meldHeap)

    val minOfHeapOne = findMin(heapOne)
    val minOfHeapTwo = findMin(heapTwo)

    minOfHeapOne == minOfHeapOne || minOfHeapOne == minOfHeapTwo
  }

  property("meldingOfTwoHeaps") = forAll { (h1 : H, h2 : H) =>

    val meldOne : H = meld(h1, h2)

    val minElement = findMin(h1)

    val heapOne = deleteMin(h1)
    val heapTwo = insert(minElement, h2)

    val meldTwo : H = meld(heapOne, heapTwo)

    val meldOneElements = getElementsInOrder(meldOne)
    val meldTwoElements = getElementsInOrder(meldTwo)

    meldOneElements == meldTwoElements

  }

  private def getElementsInOrder(theHeap : H) : List[Int] = {
    getElementsInOrder(theHeap, Nil)
  }

  private def getElementsInOrder(theHeap : H, elementsCollectedSoFar : List[Int]) : List[Int] = {

    if (isEmpty(theHeap)) elementsCollectedSoFar
    else {
      val minElement = findMin(theHeap)
      val theHeapWithoutTheMinElement = deleteMin(theHeap)

      minElement :: getElementsInOrder(theHeapWithoutTheMinElement, elementsCollectedSoFar)
    }

  }
}
