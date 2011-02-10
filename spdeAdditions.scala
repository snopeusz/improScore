/*
 * selected chunks of Enrich.scala from spde package
 */
package impro.spde

trait Colors {
  type Color = Int

  implicit def color2RichColor(color: Color) = new RichColor(color)
  
  class RichColor(color: Color) {
    def r = color >> 16 & 0xFF
    def g = color >> 8 & 0xFF
    def b = color & 0xFF
  }
}

trait Randoms {
  import scala.util.Random
  private val rand = new Random
  implicit def seq2RichRandom[K](seq: Seq[K]) = new RichRandom(seq)
  
  protected class RichRandom[K](seq: Seq[K]) {
    def random: K = seq((rand.nextFloat * seq.length).toInt)
  }
  
}

trait Maths {
  implicit def double2float(d: Double) = d.toFloat
  
  implicit def int2RicherInt(num: Int) = new RicherInt(num)
  
  protected class RicherInt(num: Int) {
    private def pow(exp: Int, acc: Int): Int = 
      if (exp == 1) acc
      else pow(exp - 1, num * acc)
    def *^ (exp: Int) = 
      if (exp > 0) pow(exp, num)
      else if (exp == 0) 1
      else error("^^ for Int returns Int; negative exponents are not allowed")
  }
}


// vim: set ts=4 sw=4 et:
