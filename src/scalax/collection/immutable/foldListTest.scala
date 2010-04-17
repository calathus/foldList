package scalax.collection.immutable

object foldListTest {
  import foldList._

  def currentTime = System.currentTimeMillis()
  def benchmark[B](title: String, f: =>B): (B, Long) = {
    val startTime = currentTime
    val v = f
    val duration = currentTime - startTime
    //println(title+", v: "+v+", time: "+duration)
    (v, duration)
  }

  def test0 {
    val x0 = foldRight((1 to 0).toList)(0)(_ + _)
    println("x0: "+x0)

    val x1 = (1 to 100).toList.foldRight(0)(_ + _)
    println("x1: "+x1)
    val x2 = foldRight((1 to 100).toList)(0)(_ + _)
    println("x2: "+x2)

    val x3a = (1 to 102).toList.foldRight(0)(_ + _)
    println("x3a: "+x3a)
    val x3b = foldRight((1 to 102).toList)(0)(_ + _)
    println("x3b: "+x3b)

    val x4a = (1 to 99).toList.foldRight(0)(_ + _)
    println("x4a: "+x4a)
    val x4b = foldRight((1 to 99).toList)(0)(_ + _)
    println("x4b: "+x4b)
  }

  def test1 {
    def test1(num: Int) {
      val es = (1 to num).toList
      val (v1, t1) = benchmark("new["+num+"]", foldRight(es)(0)(_ + _))
      //val (v2, t2) = benchmark("arr["+num+"]", es.toArray.foldRight(0)(_ + _))
      //val (v3, t3) = benchmark("vec["+num+"]", (Vector()++es).foldRight(0)(_ + _))
      //val (v3, t3) = benchmark("listb["+num+"]", (ListBuffer[Int]()++es).foldRight(0)(_ + _))
      //val (v4, t4) = benchmark("itr["+num+"]", es.iterator.foldRight(0)(_ + _))
      //println("["+num+"] new: "+t1)
      //println("["+num+"] arr: "+t2)
      println(num+" "+t1)
      //println("["+num+"] new: "+t1+", array: "+t2)
      //println("["+num+"] new: "+t1+", array: "+t2+", vec: "+t3+", iter: "+t4)
    }
    for (i <- 1 to 140) {
      test1(100000*i)
    }
  }

  def main(args: Array[String]): Unit = {
    //test0

    val (_, t) = benchmark("test1", test1)
    println("time: "+t)
  }
}
