package scalax.collection.immutable
import scala.collection.immutable.{LinearSeq, HashMap}
import scala.collection.mutable.{ArrayBuffer}

/**
 * iterator for LinearSeq. this uses minimal working space for n traversal list.
 *
 * @author <a href="http://scalathus.blogspot.com">calathus</a>
 */
object LinearSeqIterator {

  val empty = new Iterator[Nothing] {
    def hasNext: Boolean = false
    def next(): Nothing = throw new NoSuchElementException("next on empty iterator")
  }

  private def copyInterval[A](iter: Iterator[A], interval: ArrayBuffer[A], size: Int): ArrayBuffer[A] = {
    interval.reduceToSize(0)
    for (i <- 0 until size) {
      interval + iter.next
    }
    interval
  }

  private def createNodes[A](es: LinearSeq[A], es_sz: Int, sz: Int): (ArrayBuffer[LinearSeq[A]], Int, Int) = {
    val nodes = new ArrayBuffer[LinearSeq[A]](sz)   
    var es0 = es
    for (i <- 0 until es_sz) {
      if (i % sz == 0) {
        nodes + es0
      }
      es0 = es0.tail
    }
    val ndIdx = nodes.size -1
    (nodes, ndIdx, es_sz - ndIdx*sz)
  }

  def reverseIterator[A](es: LinearSeq[A], glevel: Int = 1): Iterator[A] = {
    //val A = 1024
    //val S = es.size
    //val level0 = Math.log(Math.log(S)/Math.log(A))/Math.log(2)
    //val level = Math.floor(level0).toInt
    //println(">>1 S: "+S+", glevel: "+glevel)
/*
    val a1 = Math.pow(S, Math.pow(2, -level))
    val s1 = Math.pow(a1, Math.pow(2, level))
    println(">>1 S: "+S+", level: "+level+", a1: "+a1+", s1: "+s1)
*/
    //var szArray = new ArrayBuffer[A](sz);
    def reverseIterator(es: LinearSeq[A], es_sz: Int, is_residue: Boolean, level: Int): Iterator[A] = {
      if (es_sz == 0) LinearSeqIterator.empty
      else {
        val sz: Int = Math.ceil(Math.sqrt(es_sz)).toInt
        val (nodes, ndIdx0, residue) = createNodes(es, es_sz, sz)
        lazy val szArray = new ArrayBuffer[A](sz)
        def arrayReverseIterator(es: LinearSeq[A], size: Int, is_residue: Boolean, level: Int): Iterator[A] = {
          copyInterval(es.iterator, if (is_residue && size != sz) new ArrayBuffer[A](size) else szArray, size).reverseIterator
        }

        def revIterator(f: (LinearSeq[A], Int, Boolean, Int)=>Iterator[A]): Iterator[A] =
          new Iterator[A] {
            var ndIdx = ndIdx0
            var iterator = f(nodes(ndIdx), residue, true, level-1)
            def hasNext = {
              (iterator.hasNext) || {
                ndIdx -= 1;
                (ndIdx >= 0) && {
                  iterator = f(nodes(ndIdx), sz, false, level-1)
                  hasNext
                }
              }
            }
            def next() = if (hasNext) iterator.next() else throw new NoSuchElementException()
          }

        revIterator(if (level == 0) (arrayReverseIterator _) else (reverseIterator _))
      }
    }
    reverseIterator(es, es.size, false, glevel)
  }
}
