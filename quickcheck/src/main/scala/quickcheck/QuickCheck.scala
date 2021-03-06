package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
      const(empty),
      for {
        k <- arbitrary[Int]
        m <- oneOf(const(empty), genHeap)
      } yield insert(k, m)
    )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def toStream(h: H): Stream[A] = {
    if (isEmpty(h)) Stream.Empty
    else findMin(h) #:: toStream(deleteMin(h))
  }

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == a.min(b)
  }

  property("minempty") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("sorted") = forAll { h: H =>
    toStream(h).sliding(2).forall {
      case Seq(x, y) => x <= y
      case Seq(_) => true
    }
  }

  property("merge") = forAll { (h1: H, h2: H) =>
    (toStream(h1) ++ toStream(h2)).sortBy(a => a) == toStream(meld(h1, h2)).sortBy(a => a)
  }

}
