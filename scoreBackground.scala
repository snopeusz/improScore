package impro.score

import processing.core._
import PConstants._
//import PApplet._
import impro.spde._

abstract class ViewForScore {
  val a: PApplet
  def pch2Y (pch: Float): Float 
  def pch2H (pch: Float): Float
  def beats2X (beats: Float): Float
  def beats2W (beats: Float): Float 
}

class Window(
  val a: PApplet,
  val viewWidth: Float = 600.f,
  val viewHeight: Float = 400.f,
  val xHeadRatio: Float = 0.15f,
  val yMarginRatio: Float = 0.05f
) extends ViewForScore with Maths
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
  private var _timeWindow: Float = 10.f
  private var _posOffset: Float = 1.f
  private var _posWindow: Float = 0.5f

  // in px:
  val yCenter: Float = viewHeight / 2.0f
  val yUnit: Float = scoreHeight / 40.0f
  private var xUnit: Float = scoreWidth / _timeWindow

  private var posOffsetWidth: Float = _posOffset * xUnit
  private var posWindowWidth: Float = _posWindow * xUnit
  private var posXposition: Float = headWidth + posOffsetWidth
  
  import scala.collection.mutable.ArrayBuffer
  private var _elements = new ArrayBuffer[Element]

  private var _headerElements = new ArrayBuffer[Element]

  var partID: Int = 0

  var cursorColor: Int = 1175767125

  var bypass: Boolean = false
  var drawGrid: Boolean = true

  //dumpVars

  def pos = _pos
  def pos_= (newPos: Float) {
    _pos = newPos
    // [removed] - czyszczenie _elements
    //((_elements.toArray filter (e => e.end < _pos)) 
    //  foreach (e => _elements -= e))
  }

  // calculate and cache vars dependant on other parms
  private def calculateInternals {
    xUnit = scoreWidth / _timeWindow
    posOffsetWidth = _posOffset * xUnit
    posWindowWidth = _posWindow * xUnit
    posXposition = headWidth + posOffsetWidth
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
    if (newOffset >= 0.f && newOffset < _timeWindow) {
      _posOffset = newOffset
      calculateInternals
    }
    else error("posOffset: " + newOffset +
        " has to be in range 0 - timeWindow(" + _timeWindow + ")")
  }

  def posWindow = _posWindow
  def posWindow_= (newWindow: Float) {
    if (newWindow > 0.f) {
      _posWindow = newWindow
      calculateInternals
    }
  }

  // --- available in Elements' 'draw' method --- //
  def pch2Y (pch: Float): Float = 
    viewY + yCenter + (pch * yUnit * -1.f)
  def pch2H (pch: Float): Float = yUnit * pch
  def beats2X (beats: Float): Float =
    viewX + headWidth + posOffsetWidth +
    ((beats - _pos) * xUnit)
  def beats2W (beats: Float): Float = xUnit * beats
  // -------------------------------------------- //

  val temp_a = a
  val headerWindow = new ViewForScore {
    val a: PApplet = temp_a
    def pch2Y (pch: Float): Float = 
      viewY + yCenter + (pch * yUnit * -1.f)
    def pch2H (pch: Float): Float = yUnit * pch
    // header window: beats 0..1 => Left..Right
    def beats2X (beats: Float): Float =
      viewX + (headWidth * beats)
    def beats2W (beats: Float): Float = headWidth * beats
  } 

  /*
  rysuje partyturę na wyznaczonym obszarze:
  -tło: rejestry, pięciolinie
  -początek: ewentualne klucze
  -kolejne elementy notacji
  -kursor

  */
  def draw {
    if (!bypass) {
      // --- borders of view
      //a.rect(viewX, viewY, viewWidth, viewHeight)
      //val headScoreBorderX = viewX + headWidth
      //a.line(headScoreBorderX, viewY,headScoreBorderX, viewY+viewHeight)

      // --- cursor:
      a.pushStyle
      //a.fill(20, 200, 85, 40)
      a.fill(cursorColor)
      a.noStroke
      a.rect(viewX + posXposition, viewY, posWindowWidth, viewHeight)
      a.popStyle

      // +++ grid
      if (drawGrid) {
        a.pushStyle
        a.stroke(200)
        a.strokeWeight(yUnit * 0.5)
        //val x0 = viewX + headWidth + (((posOffset % 1) - (pos % 1)) * xUnit)
        //val x0 = viewX + headWidth + (((pos - posOffset) % 1) * xUnit)
        val x0 = viewX + headWidth + posOffsetWidth - ((pos % 1) * xUnit)
        //...
        for (x <- 0 until _timeWindow.round)
          a.line(x0 + (x * xUnit), viewY, x0 + (x * xUnit), viewY + viewHeight)
        a.popStyle
      }

      // *** SCORE ELEMENTS
      for {
        element <- _elements.toArray
        if ((element.beg < _pos + _timeWindow - _posOffset) 
            && (element.end > _pos - _posOffset))
      } element.draw(this)

      // todo: gradient za nagłówkiem (white->transparent)
      a.pushStyle
      a.fill(255)
      a.noStroke
      a.rect(viewX, viewY, headWidth, viewHeight)
      a.popStyle

      // *** HEADER ELEMENTS
      for {
        element <- _headerElements.toArray
      } element.draw(headerWindow)

      // ...
    }
  }

  def addElement(newElement: Element) {
    if (newElement.end > _pos) {
      _elements += newElement
      // DEBUG:
      println("added SCORE Element: " + newElement.toString)
    }
  }

  def addHeaderElement(newElement: Element) {
    if (newElement.beg < 1.0) {
      _headerElements += newElement
      // DEBUG:
      println("added HEADER Element: " + newElement.toString)
    }
  }

  // TODO: clear only selected time area (beg,dur)
  def clearScore = _elements.clear()

  def clearHeader = _headerElements.clear()
  

  def elementsNumber :Int = _elements.size
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
