package scalax.collection.immutable
import scala.collection.immutable.{LinearSeq, HashMap}

trait Benchmark {
  def currentTime = System.currentTimeMillis()
  def benchmark[B](title: String, f: =>B): (B, Long) = {
    val startTime = currentTime
    val v = f
    val duration = currentTime - startTime
    //println(title+", v: "+v+", time: "+duration)
    (v, duration)
  }
}
