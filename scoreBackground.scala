package impro.score

import processing.core._
import PConstants._
//import PApplet._
import impro.spde._

/*
Zbiera nuty i wyświetla w oknie - generuje score.Element-y (metody addElement(...) )
wyświetla kursor
przechowuje aktualną pozycję

*/


class Window(
  val a: PApplet,
  val viewWidth: Float = 600.f,
  val viewHeight: Float = 400.f,
  val xHeadRatio: Float = 0.15f,
  val yMarginRatio: Float = 0.05f
) extends Maths
{
  assert(xHeadRatio < 1.f, "xHeadRatio smaller than 1")
  assert(yMarginRatio < 1.f, "yMarginRatio smaller than 1")

  // in px:
  var viewX: Float = 10.f
  var viewY: Float = 10.f

  val headWidth: Float = xHeadRatio * viewWidth
  val scoreWidth: Float = viewWidth - headWidth
  val yMargin: Float = viewHeight * yMarginRatio
  val scoreHeight: Float = viewHeight - (2.f * yMargin)

  // in beats:
  private var _pos: Float = 0.f
  private var _timeWindow: Float = 30.f
  private var _posOffset: Float = 1.f
  private var _posWindow: Float = 0.5f

  // in px:
  val yCenter: Float = viewHeight / 2.0f
  val yUnit: Float = scoreHeight / 40.0f
  private var xUnit: Float = scoreWidth / _timeWindow

  private var posOffsetWidth: Float = _posOffset * xUnit
  private var posWindowWidth: Float = _posWindow * xUnit
  
  import scala.collection.mutable.ArrayBuffer
  private var _elements = new ArrayBuffer[Element]

  //dumpVars

  // calculate and cache vars dependant on other parms
  private def calculateInternals {
    xUnit = scoreWidth / _timeWindow
    posOffsetWidth = _posOffset * xUnit
    posWindowWidth = _posWindow * xUnit
  }

  // timeWindow: how much a performer see
  def timeWindow: Float = _timeWindow
  def timeWindow_= (newTW: Float) {
    if (newTW > 0.f) {
      _timeWindow = newTW
      calculateInternals
    }
    else error("timeWindow must be positive number!")
  }

  def posOffset = _posOffset
  def posOffset_= (newOffset: Float) {
    if (newOffset > 0.f && newOffset < _timeWindow) {
      _posOffset = newOffset
      calculateInternals
    }
    else error("posOffset: " + newOffset +
        " has to be in range 0 - timeWindow(" + _timeWindow + ")")
  }


  // --- available in Elements' 'draw' method --- //
  def pch2Y (pch: Float): Float = 
    viewY + yCenter + (pch * yUnit)
  def pch2H (pch: Float): Float = yUnit * pch
  def beats2X (beats: Float): Float =
    viewX + headWidth + posOffsetWidth +
    ((beats - _pos) * xUnit)
  def beats2W (beats: Float): Float = xUnit * beats
  // --------------------------------------------

  /*
  rysuje partyturę na wyznaczonym obszarze:
  -tło: rejestry, pięciolinie
  -początek: ewentualne klucze
  -kolejne elementy notacji
  -kursor

  */
  def draw {
    a.rect(viewX, viewY, viewWidth, viewHeight)
    // elements in score area:
    for (element <- _elements.toArray)
      element.draw(this)
  }

  def addElement(newElement: Element) {
    val beg = newElement.beg
    val addAtIndex = (_elements indexWhere (e => { beg > e.beg })) + 1
    println("adding new element at " + addAtIndex + 
      ", beg=" + beg)
    _elements insert (addAtIndex, newElement)
  }

  /*
  def setPos (newPos: Float) {
    pos = newPos
    val removeIndex = elements indexWhere (
      e => { (e.beg + e.dur) >= newPos }
    )
    elements trimStart (removeIndex - 1)
  }
  */


  // debugging:
  def dumpVars {
     replutils.printAttrValues(this)
    println("xUnit = " + xUnit)
    println("yUnit = " + yUnit)
    println("posOffsetWidth = " + posOffsetWidth)
    println("posWindowWidth = " + posWindowWidth)
    println("_timeWindow = " + _timeWindow)
    println("_pos = " + _pos)
    println("_posOffset = " + _posOffset)
    println("_posWindow = " + _posWindow)
    println("_elements = " + _elements)
    for (element <- _elements.toArray)
      element.dumpVarsWithView(this)
  }


}
