package impro.score

import processing.core._
import PConstants._
import PApplet._

import impro.spde._

abstract class Element extends Maths
{
  val beg: Float 
  val dur: Float 
  val params: Map[String, Any]

  def end: Float = beg + dur

  /* rysuje w określonym miejscu */
  def draw(v: Window) 

  def dumpVarsWithView(v: Window) {
    replutils.printAttrValues(this)
  }
}

class SimpleNote (
  val beg: Float,
  val dummydur: Float,
  val params: Map[String, Any]
) extends Element  {
  val dur = 1.f
  val pchStart: Float = params.getOrElse("pchStart", 0.f).asInstanceOf[Float]
  val fillcolor: Int = params.getOrElse("fillColor", 0).asInstanceOf[Int]

  // debug:
  override def dumpVarsWithView(v: Window) {
    replutils.printAttrValues(this)
    println("-")
    println("x: " + v.beats2X(beg))
    println("y: " + v.pch2Y(pch))
    println("--")
  }

 
  def draw(v: Window) 
  {
    val x = v.beats2X(beg)
    //val x1 = v.beats2W(1.f)
    val y = v.pch2Y(pchStart)
    val hh = v.pch2H(0.5f)
    val x1 = v.pch2H(1.1f)
    v.a.pushStyle
    v.a.fill(fillcolor)
    v.a.ellipse(x, y - hh, x1,  hh)
    v.a.popStyle
  }
}
