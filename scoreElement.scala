package impro.score

import processing.core._
import PConstants._
import PApplet._

import impro.spde._

abstract class Element extends Maths
{
  val beg: Float 
  val dur: Float 
  val params: Map[String, Float]

  def end: Float = beg + dur

  /* rysuje w określonym miejscu */
  def draw(v: Window) 

  def dumpVarsWithView(v: Window)
}

class SimpleNote (
  val beg: Float,
  val dur: Float,
  val params: Map[String, Float]
) extends Element  {
  val pchStart: Float = params.getOrElse("pchStart", 0.f)

  // debug:
  def dumpVarsWithView(v: Window) {
    replutils.printAttrValues(this)
    println("-")
    println("x: " + v.beats2X(beg))
    println("y: " + v.pch2Y(pchStart))
    println("--")
  }

 
  def draw(v: Window) 
  {
    val x = v.beats2X(beg)
    val x1 = v.beats2W(1.f)
    val y = v.pch2Y(pchStart)
    val hh = v.pch2H(0.3f)
    v.a.pushStyle
    v.a.fill(0)
    //v.a.rect(x, y - hh, x1,  hh)
    v.a.ellipse(x, y - hh, x1,  hh)
    v.a.popStyle
  }
}
