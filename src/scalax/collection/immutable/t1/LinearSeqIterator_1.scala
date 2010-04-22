package scalax.collection.immutable.t1
import scala.collection.immutable.{LinearSeq, HashMap}

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

  private def copyInterval[A: Manifest](iter: Iterator[A], interval: Array[A]): Array[A] = {
    val size = interval.size
    for (i <- 0 until size) {
      interval(i) = iter.next
    }
    interval
  }

  private def createNodes[A: Manifest](es: LinearSeq[A], es_sz: Int, sz: Int): (Array[LinearSeq[A]], Int, Int) = {
    val nodes = new Array[LinearSeq[A]](sz)
    var ndIdx = 0
    var es0 = es
    for (i <- 0 until es_sz) {
      if (i % sz == 0) {
        nodes(ndIdx) = es0
        ndIdx += 1
      }
      es0 = es0.tail
    }
    ndIdx -= 1
    (nodes, ndIdx, es_sz - ndIdx*sz)
  }

  def newGetArray[A: Manifest]() = {
    var map = new HashMap[Int, Array[A]]();
    (sz: Int)=>map.get(sz) match {
        case Some(x) => x
        case None => {
          val arr = new Array[A](sz)
          map += sz -> arr
          arr
        }
      }
    }

  def reverseIterator[A: Manifest](es: LinearSeq[A], glevel: Int = 1): Iterator[A] = {
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
    def reverseIterator(es: LinearSeq[A], es_sz: Int, level: Int): Iterator[A] = {
      if (es_sz == 0) LinearSeqIterator.empty
      else {
        val sz: Int = Math.ceil(Math.sqrt(es_sz)).toInt
        val (nodes, ndIdx0, residue) = createNodes(es, es_sz, sz)
        val getArray = if (glevel <= 1) {
            var szArray = new Array[A](sz);
            (size: Int)=>if (size == sz) szArray else new Array[A](size)
          } else newGetArray[A]()
        def arrayReverseIterator(es: LinearSeq[A], size: Int, level: Int): Iterator[A] = {
          copyInterval(es.iterator, getArray(size)).reverseIterator
        }

        def revIterator(f: (LinearSeq[A], Int, Int)=>Iterator[A]): Iterator[A] =
          new Iterator[A] {
            var ndIdx = ndIdx0
            var iterator = f(nodes(ndIdx), residue, level-1)
            def hasNext = {
              (iterator.hasNext) || {
                ndIdx -= 1;
                (ndIdx >= 0) && {
                  iterator = f(nodes(ndIdx), sz, level-1)
                  hasNext
                }
              }
            }
            def next() = if (hasNext) iterator.next() else throw new NoSuchElementException()
          }

        revIterator(if (level == 0) (arrayReverseIterator _) else (reverseIterator _))
      }
    }
    reverseIterator(es, es.size, glevel)
  }
}
