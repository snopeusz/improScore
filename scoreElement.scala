package impro.element

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



*/
case class ScoreElement (beg: Float, dur: Float, startPch: Float, endPch: Float) extends Maths
{

  /* ilość pikseli na jedną sekundę/miarę (dur) */
  val xscale :Float = 100.0f
  /* ilość pikseli od nawyższego dźw. do najniższego */
  val yscale :Float = 200.0f
  /* funkcja przeliczająca wysokość na pionowe położenie */
  // pch = (0..100)
  var pch2vertPos :(Float => Float) = 
  (pch: Float) => ((1.0f - (pch * 0.01f)) * yscale)

  private var _img: Option[PImage] = None

  /* rysuje w określonym miejscu */
  def drawAt(app: PApplet, origX: Float, origY: Float) {
    val x0 = origX
    val x1 = (dur * xscale) + origX
    val y0 = origY + pch2vertPos(startPch)
    val y1 = origY + pch2vertPos(endPch)
    app.smooth
    app.strokeWeight(4)
    app.stroke(0)
    app.line(x0, y0, x1, y1)
  }

  def drawAtGC(gc: PGraphics) {
    val x0 = 0.f
    val x1 = (dur * xscale) 
    val y0 =  pch2vertPos(startPch)
    val y1 =  pch2vertPos(endPch)
    gc.smooth
    gc.strokeWeight(4)
    gc.stroke(0)
    gc.line(x0, y0, x1, y1)
  }
  /* @return PImage */
  def makeImg  {
    val pgWidth = (xscale * dur)
    val pgHeight =
      //(pch2vertPos(startPch) - pch2vertPos(endPch))
      scala.math.max(pch2vertPos(startPch), pch2vertPos(endPch))
    val pg = new PGraphicsJava2D
    pg.setSize(pgWidth.toInt, pgHeight.toInt)
    pg.beginDraw
    pg.background(255,255)
    drawAtGC(pg)
    pg.endDraw
    _img = Some(pg.asInstanceOf[PImage])
    //_img = Some(pg)
  }

  def img : PImage = {
    if (_img.isEmpty) makeImg
    _img.orNull.get
  }
}

