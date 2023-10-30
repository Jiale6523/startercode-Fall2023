case class Neumaier(sum: Double, c: Double)

object HW {

   def q1_countsorted(x: Int, y: Int, z:Int) = {
    var count = 0
    if(x < y) count += 1
    if(y < z) count += 1
    if(x < z) count += 1
    count
   }

   def q2_interpolation(name: String, age: Int) = {
      if(age >= 21) s"hello, ${name.toLowerCase}" else s"howdy, ${name.toLowerCase}"
      
   }

   def q3_polynomial(arr: Seq[Double]) = {
      arr.foldLeft((0.0, 0))((acc, elem) => (acc._1 + (elem * Math.pow(2, acc._2)), acc._2 + 1))._1
      
   }

   def q4_application(x: Int, y: Int, z: Int)(f: (Int, Int) => Int) = {
      f(f(x, y), f(y, z))
   }
   
   def q5_stringy(start: Int, n: Int): Vector[String] = {
    Vector.tabulate(n)(i => s"(${start + i})")
   }

   def q6_modab(a: Int, b: Int, c: Vector[Int]): Vector[Int] = {
    c.filter(i => i >= a && i % b != 0)
   }

   def q7_find(vec: Vector[Int], f: Int => Boolean): Int = {
    vec.lastIndexWhere(f)
   }

   def q8_find_tail(vec: Vector[Int], f: Int => Boolean, index: Int = -1, currentIndex: Int = 0): Int = {
    if(vec.isEmpty) index
    else {
      val newIndex = if(f(vec.head)) currentIndex else index
      q8_find_tail(vec.tail, f, newIndex, currentIndex + 1)
       }
   }

   case class Neumaier(sum: Double, c: Double)
  
  def q9_neumaier(input: Seq[Double]): Double = {
    val result = input.foldLeft(Neumaier(0.0, 0.0)) { (acc, elem) =>
      val t = acc.sum + elem
      if(Math.abs(acc.sum) >= Math.abs(elem)) {
        Neumaier(t, acc.c + (acc.sum - t + elem))
      } else {
        Neumaier(t, acc.c + (elem - t + acc.sum))
         }
       }
    result.sum + result.c
     }
   }

   
   // create the rest of the functions yourself
   // in order for the code to compile, you need to (at the very least) create
   // blank versions of the remaining functions and have them return a value of 
   // the expected type, like the blank functions above.
   // remember, to compile, you don't specify any file names, you just use sbt compile
}
