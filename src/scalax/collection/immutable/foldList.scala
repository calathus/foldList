package scalax.collection.immutable
import scala.collection.mutable._

object foldList {

  def foldRight[A: Manifest, B](es: List[A])(z: B)(op: (A, B) => B): B = {

    def copyInterval(es: List[A], interval: Array[A]): Array[A] = {
      val size = interval.size
      val iter = es.iterator
      for (i <- 0 until size) {
        interval(i) = iter.next
      }
      interval
    }

    def createNodes(es: List[A], es_sz: Int, sz: Int): (Array[List[A]], Int, Int) = {
      val nodes = new Array[List[A]](sz)
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

    var map = new HashMap[Int, Array[A]]()
    def getArray(sz: Int): Array[A] = {
      map.get(sz) match {
        case Some(x) => x
        case None => {
          val arr = new Array[A](sz)
          map += sz -> arr
          arr
        }
      }
    }

    //
    // the number of list traversal is approximately level+1
    // the size of final interval(a) is exp(s, exp(2, -level)) where s is the size of original list
    // if we fix a, the required level is log(2, log(a, s))
    //
    def foldr(z: B, es: List[A], es_sz: Int, level: Int): B = {
      if (es_sz == 0) {
        z
      } else {
        val sz: Int = Math.ceil(Math.sqrt(es_sz)).toInt
        val (nodes, ndIdx0, residue) = createNodes(es, es_sz, sz)
        var ndIdx = ndIdx0
        if (level == 0) {
          var interval = getArray(residue)
          var z1 = copyInterval(nodes(ndIdx), interval).foldRight(z)(op)
          if (residue != sz) interval = getArray(sz)

          while ({ndIdx -= 1; ndIdx >= 0}) {
            z1 = copyInterval(nodes(ndIdx), interval).foldRight(z1)(op)
          }
          z1
        } else {
          var z1 = foldr(z, nodes(ndIdx), residue, level-1)
          while ({ndIdx -= 1; ndIdx >= 0}) {
            z1 = foldr(z1, nodes(ndIdx), sz, level-1)
          }
          z1
        }
      }
    }

    val A = 1000
    val S = es.size
    val level0 = Math.log(Math.log(S)/Math.log(A))/Math.log(2)
    val level = Math.floor(level0).toInt

/*
    val a1 = Math.pow(S, Math.pow(2, -level))
    val s1 = Math.pow(a1, Math.pow(2, level))
    println(">>1 S: "+S+", level: "+level+", a1: "+a1+", s1: "+s1)
*/
    val res = foldr(z, es, S, level)
    res
  }
}
