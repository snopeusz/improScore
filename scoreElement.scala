package impro.score

import processing.core._
import PConstants._
import PApplet._

import impro.spde._
/*
Najprostsza wersja: linia o stałej grubości.
pola: 
beg - początek, 
dur - długość, 
startPch - wysokość początkowa, 
endPch - wysokość końcowa
startDyn - początkowa dynamika
endDyn - dynamika końcowa

TODO: dodać parametr kontstruktora 'scoreWindow' - obiekt score.Window
      z aktualnym oknem – pobierać z niedo wszystkie informacje o kontekście: x/y scale, pch2vertPos, app

      
  startPch: Float = 0.f, endPch: Float = 0.f, 
  startDyn: Float = 0.5f, endDyn: Float = 0.5f
  beg: Float = 0.f, dur: Float = 0.0f, 
  val params: Map[String, Float] = Map[String, Float]()
*/
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
