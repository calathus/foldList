package scalax.collection.immutable
import scala.collection.immutable.{LinearSeq, HashMap}

/**
 * iterator for LinearSeq. this uses minimal working space for n traversal list.
 *
 * @author <a href="http://scalathus.blogspot.com">calathus</a>
 */
object LinearSeqIteratorTest extends Benchmark {
  import LinearSeqIterator._

  def test0[A](revIter: Iterator[A]) {
    while (revIter.hasNext) {
      val v = revIter.next
      //println(">> "+v)
    }
  }

  def test1 {
    def test1(num: Int) {
      val es = (1 to num).toList
      val (v1, t1) = benchmark("new["+num+"]", test0(reverseIterator(es)))
      //val (v1, t1) = benchmark("reviter["+num+"]", test0(es.reverseIterator))
      //val (v2, t2) = benchmark("reviter["+num+"]", es.reverseIterator.foldLeft(0)(_ + _))
      //val (v1, t1) = benchmark("new["+num+"]", revIter.foldLeft(0)(_ + _))
      //val (v2, t2) = benchmark("reviter["+num+"]", es.reverseIterator.foldLeft(0)(_ + _))
      println(num+" "+t1)
    }

    for (i <- 1 to 140) {
      test1(100000*i)
    }
  }

  def main(args: Array[String]): Unit = {
    //val es = (1 to 99).toList
    //test0(reverseIterator(es))

    val (_, t) = benchmark("test1", test1)
    println("time: "+t)
  }
}
