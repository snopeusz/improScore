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
  def draw(v: ViewForScore) 

  def dumpVarsWithView(v: Window) {
    replutils.printAttrValues(this)
  }
}

case class SimpleNote (
  val beg: Float,
  val dummydur: Float,
  val params: Map[String, Any]
) extends Element  {
  val dur = 1.f
  val pch: Float = params.getOrElse("pch", 0.f).asInstanceOf[Float]
  val color: Int = params.getOrElse("color", 0).asInstanceOf[Int]
  val dyn: Float = params.getOrElse("dyn", 0.f).asInstanceOf[Float]

  // debug:
  override def dumpVarsWithView(v: Window) {
    replutils.printAttrValues(this)
    println("-")
    println("x: " + v.beats2X(beg))
    println("y: " + v.pch2Y(pch))
    println("--")
  }

 
  def draw(v: ViewForScore) 
  {
    //val x1 = v.beats2W(1.f)
    val yu = v.pch2H(1.0f)
    val w = yu * 2.f * dyn
    val h = yu * 1.5f * dyn
    val x = v.beats2X(beg) + (w * 0.5f)
    val y = v.pch2Y(pch)
    v.a.pushStyle
    v.a.fill(color)
    //v.a.ellipseMode(CORNER)
    v.a.ellipse(x, y , w, h)
    v.a.popStyle
  }
}

case class SimpleLine (
  val beg: Float,
  val dur: Float,
  val params: Map[String, Any]
) extends Element  {
  val pch: Float = params.getOrElse("pch", 0.f).asInstanceOf[Float]
  val pchEnd: Float = params.getOrElse("pchEnd", pch).asInstanceOf[Float]
  val color: Int = params.getOrElse("color", 0).asInstanceOf[Int]
  val dyn: Float = params.getOrElse("dyn", 0.f).asInstanceOf[Float]

  def draw(v: ViewForScore) 
  {
    //val x1 = v.beats2W(1.f)
    val yu = v.pch2H(1.0f)
    val x = v.beats2X(beg)
    val y = v.pch2Y(pch)
    val x1 = v.beats2X(end)
    val y1 = v.pch2Y(pchEnd)
    val lineWidth = yu * dyn
    v.a.pushStyle
    v.a.fill(color)
    v.a.strokeWeight(lineWidth)
    v.a.line(x, y, x1, y1)
    v.a.popStyle
  }
}

case class Staff5 (
  val beg: Float,
  val dur: Float,
  val params: Map[String, Any]
) extends Element  {
  
  val color: Int = params.getOrElse("color", 0).asInstanceOf[Int]

  def draw(v: ViewForScore) 
  {
    val x = v.beats2X(beg)
    val x1 = v.beats2X(end)
    val y = v.pch2Y(0.f)
    val h = v.pch2H(1.f)
    v.a.pushStyle
    v.a.stroke(color)
    v.a.strokeWeight(h * 0.2f)
    v.a.line(x, y , x1, y)
    v.a.line(x, y + (2*h) , x1, y + (2*h))
    v.a.line(x, y + (4*h) , x1, y + (4*h))
    v.a.line(x, y - (2*h) , x1, y - (2*h))
    v.a.line(x, y - (4*h) , x1, y - (4*h))
    v.a.popStyle
  }
}

case class StaffReg3 (
  val beg: Float,
  val dur: Float,
  val params: Map[String, Any]
) extends Element  {
  val color: Int = params.getOrElse("color", 2139062271).asInstanceOf[Int]
  def draw(v: ViewForScore) 
  {
    val x = v.beats2X(beg)
    val x1 = v.beats2X(end)
    val y = v.pch2Y(0.f)
    val h = v.pch2H(1.f)
    v.a.pushStyle
    v.a.stroke(color)
    v.a.strokeWeight(h * 0.2f)
    v.a.line(x, y + (20*h) , x1, y + (20*h))
    v.a.line(x, y + (7*h) , x1, y + (7*h))
    v.a.line(x, y - (20*h) , x1, y - (20*h))
    v.a.line(x, y - (7*h) , x1, y - (7*h))
    v.a.popStyle
  }
}
