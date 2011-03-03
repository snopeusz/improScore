package impro

import processing.core._
import PConstants._

import impro.spde._

import oscP5._
import netP5._

import impro._

object ScoreRunner {
  def main(args: Array[String]) {
    //PApplet.main(args ++ Array("--present", "impro.Score") ) 
    PApplet.main(args  ++ Array("impro.ScoreDisplay") ) 
  }
}

class ScoreDisplay extends PApplet with Colors with Maths with Randoms {

  import PApplet._


  lazy val oscP5 = new OscP5(this, 12000)
  //val myRemoteLocation = new NetAddress("127.0.0.1", 12000)
  val myRemoteLocation = new NetAddress("127.0.0.1", 57120)

  //val scoreView = new score.Window(this, 1280, 500, 0.1f) 
  //lazy val scoreView = new score.Window(this, width, height, 0.1f) 

  var scores :Array[score.Window] = _

  lazy val fontReg = loadFont("lib/TeXGyreTermes-Regular-48.vlw")
  lazy val fontBI = loadFont("lib/TeXGyreTermes-BoldItalic-48.vlw")

  //scoreView.dumpVars

  /** actual score position */
  var pos: Float = 0.f
  /** movement of notation */
  var speed: Float = 1.f
  var actual_speed: Float = 1.f
  var nextPos: Float = pos + actual_speed
  var gravity: Float = 0.2f

  var drawDebugInfo = true


  override def setup() {
    size(1280, 500, P2D)
    frameRate(30)
    smooth
    frame.setTitle("The Score")
    fontBI // lazy FontBI
    textFont(fontReg) // lazy fontReg eval + select as default font

    // -- oscP5
    oscP5 // właściwe inicjowanie (lazy)

    // -- scores
    val _scores = scala.collection.mutable.ArrayBuffer[score.Window]()

    val scoreView = new score.Window(this, width, height / 2, 0.1f) 
    prepareDummyDataForTests(scoreView)
    scoreView.viewX = 0
    scoreView.viewY = 0
    scoreView.partID = 0
    _scores += scoreView // add scores to general list

    val scoreView2 = new score.Window(this, width, height / 2, 0.1f) 
    prepareDummyDataForTests(scoreView2)
    scoreView2.viewX = 0
    scoreView2.viewY = height / 2
    scoreView2.partID = 1
    _scores += scoreView2 // add scores to general list

    scores = _scores.toArray
  }

  override def draw() {
    val scoreView = scores(0)
    val scoreView2 = scores(1)
    background(255)
    scoreView.pos = pos
    scoreView2.pos = pos
    scoreView.draw
    scoreView2.draw


    if (drawDebugInfo) {
      pushStyle
      fill(0)
      text(frameRate.toString take 6, 10, 150);
      fill(55,0,0)
      text(pos.toString take 6, 10, 100);
      fill(0,55,0)
      text(scoreView.elementsNumber.toString, 10, 50);
      noFill
      stroke(255,0,0)
      strokeWeight(10)
      line(0, scoreView2.viewY, width, scoreView2.viewY)
      popStyle
    }
    
    actual_speed += (speed - actual_speed) * gravity
    if (frameRate > 0.f)
      pos += actual_speed / frameRate

  }

  // tests...
  //override def mousePressed() = scoreView.clearHeader
  
  /*
  override def mousePressed() {
    val myMessage  = new OscMessage("/test")
    myMessage.add(123)
    oscP5.send(myMessage, myRemoteLocation)
  }
  */

  /* ==== Utility methods for handling OSC traffic ==== */
  // general purpose function
  def rsplit2(str: String, list: List[String] = List()): List[String] = {
    val (str1, str2) = str splitAt 2
    if (str2.size < 2)
      return (str1 :: list).reverse
    else
      rsplit2(str2, str1 :: list)
  }

  private def elementTypetag2Props (msg:OscMessage): Map[String, Any] = {
    var props = scala.collection.mutable.Map[String, Any]()
    rsplit2(msg.typetag drop 3).zipWithIndex foreach { 
      t => t._1 match {
        case "si" => props += (msg.get(t._2 * 2 + 3).stringValue -> 
          msg.get(t._2 * 2 + 4).intValue)
        case "sf" => props += (msg.get(t._2 * 2 + 3).stringValue -> 
          msg.get(t._2 * 2 + 4).floatValue)
        case "ss" => props += (msg.get(t._2 * 2 + 3).stringValue -> 
          msg.get(t._2 * 2 + 4).stringValue)
        case _ =>
      }
    }
    props.toMap
  }

  private def msgToElement (elemName: String, msg: OscMessage): Option[score.Element] = {
    (names2elements get elemName) match {
      case Some(elem) if (msg.typetag.startsWith("iff")) => { 
        val partID = msg.get(0).intValue
        val beg = msg.get(1).floatValue
        val dur = msg.get(2).floatValue
        val props :Map[String, Any] = elementTypetag2Props(msg)
        Some(elem(beg, dur, props))
      }
      case _ => None
    }
  }

  // proper handling of OSC traffic
  private def msgSync (msg: OscMessage, theViews: Iterable[score.Window]) {
    println("syncing!")
    if (msg.typetag startsWith "iff") {
      val nextPos = msg.get(1).floatValue
      val time2next = msg.get(2).floatValue
      speed = (nextPos - pos) / time2next

    }
  }

  private def msgSetGravity (msg: OscMessage, theViews: Iterable[score.Window]) {
    println("set gravity!")
    if (msg.typetag startsWith "if") 
      gravity = msg.get(1).floatValue
  }
  private def msgSetSpeed (msg: OscMessage, theViews: Iterable[score.Window]) {
    println("set speed!")
    if (msg.typetag startsWith "if") 
      speed = msg.get(1).floatValue
  }

  private def msgSetPos (msg: OscMessage, theViews: Iterable[score.Window]) {
    println("set pos!")
    if (msg.typetag startsWith "if") 
      pos = msg.get(1).floatValue
  }

  private def msgAddElement (
    elemName: String, msg: OscMessage, theView: score.Window
  ) {
    msgToElement(elemName, msg) match {
      case Some(elem) => theView.addElement(elem)
      case _ =>
    }
  }

  private def msgAddElementToHeader (
    elemName: String, msg: OscMessage, theView: score.Window
  ) {
    msgToElement(elemName, msg) match {
      case Some(elem) => theView.addHeaderElement(elem)
      case _ =>
    }
  }


  private def msgClear (msg: OscMessage, theView: score.Window) {
    println("clear score!")
    theView.clearScore
    // TODO: clear only selected time area (beg,dur)
  }

  private def msgClearHeader (msg: OscMessage, theView: score.Window) {
    println("clear header!")
    theView.clearHeader
  }

  private def msgSetTimeWindow (msg: OscMessage, theView: score.Window) {
    println("set timeWindow!")
    if (msg.typetag()(1) == 'f')
      theView.timeWindow = msg.get(1).floatValue
  }
  private def msgSetPosWindow (msg: OscMessage, theView: score.Window) {
    println("set posWindow!")
    if (msg.typetag()(1) == 'f')
      theView.posWindow = msg.get(1).floatValue
  }
  private def msgSetPosOffset (msg: OscMessage, theView: score.Window) {
    println("set posOffset!")
    if (msg.typetag()(1) == 'f')
      theView.posOffset = msg.get(1).floatValue
  }
  private def msgSetBypass (msg: OscMessage, theView: score.Window) {
    println("set bypass!")
    if (msg.typetag()(1) == 'i')
      theView.bypass = (msg.get(1).intValue > 0)
  }
  private def msgSetGrid (msg: OscMessage, theView: score.Window) {
    println("set grid!")
    if (msg.typetag()(1) == 'i')
      theView.drawGrid = (msg.get(1).intValue > 0)
  }
  private def msgDrawDebugInfo (msg: OscMessage, theViews: Iterable[score.Window]) {
    println("set draw debug info!")
    if (msg.typetag.size == 1)
      drawDebugInfo = !drawDebugInfo
    else if (msg.typetag()(1) == 'i')
      drawDebugInfo = (msg.get(1).intValue > 0)
  }

  // this should be in scoreElement.scala, shouldn't it ?
  val names2elements = Map(
    "simpleLine"    -> score.SimpleLine,
    "simpleNote"    -> score.SimpleNote,
    "barLine"       -> score.BarLine,
    "text"          -> score.Text,
    "textBI"        -> score.TextBI,
    "staff5"        -> score.Staff5,
    "staffReg3"     -> score.StaffReg3
  )

  import scala.util.matching.Regex
  val OSCADDR_RE_ADD = new Regex("""/imps/add/(.+)""")
  val OSCADDR_RE_HADD = new Regex("""/imps/hadd/(.+)""")
  val OSCADDR_SYNC = "/imps/sync"
  val OSCADDR_CLEAR = "/imps/clear"
  val OSCADDR_HCLEAR = "/imps/hclear"
  val OSCADDR_SETPOS = "/imps/setPos"
  val OSCADDR_SETTIMEWIN = "/imps/setTimeWin"
  val OSCADDR_SETPOSWIN = "/imps/setPosWin"
  val OSCADDR_SETPOSOFFSET = "/imps/setPosOffset"
  val OSCADDR_SETSPEED = "/imps/setSpeed"
  val OSCADDR_SETGRAVITY = "/imps/setGravity"
  val OSCADDR_SETBYPASS = "/imps/setBypass"
  val OSCADDR_SETGRID = "/imps/setGrid"
  val OSCADDR_DRAWDEBUGINFO = "/imps/drawDebugInfo"

  // *** MAIN OSC RESPONDER:
  def oscEvent(msg :OscMessage) {
    val addr = msg.addrPattern
    val tt = msg.typetag
    val ip = msg.address
    val port = msg.port

    if (addr.startsWith("/imps") && tt.startsWith("i")) {
      val partID :Int = msg.get(0).intValue
      val ss = if (partID == -1) scores
        else scores filter (_.partID == partID)
      addr match {
        // -- global settings (for every view)
        case OSCADDR_SYNC =>  msgSync(msg, ss)
        case OSCADDR_SETPOS => msgSetPos(msg, ss)
        case OSCADDR_SETGRAVITY =>  msgSetGravity(msg, ss)
        case OSCADDR_SETSPEED => msgSetSpeed(msg, ss)
        case OSCADDR_DRAWDEBUGINFO => msgDrawDebugInfo(msg, ss)
        // -- settings for specific view:
        case OSCADDR_CLEAR => ss foreach (msgClear(msg, _))
        case OSCADDR_HCLEAR => ss foreach (msgClearHeader(msg, _))
        case OSCADDR_SETTIMEWIN => ss foreach (msgSetTimeWindow(msg, _))
        case OSCADDR_SETPOSWIN => ss foreach (msgSetPosWindow(msg, _))
        case OSCADDR_SETPOSOFFSET => ss foreach (msgSetPosOffset(msg, _))
        case OSCADDR_SETBYPASS => ss foreach (msgSetBypass(msg, _))
        case OSCADDR_SETGRID => ss foreach (msgSetGrid(msg, _))
        case OSCADDR_RE_ADD(elem) => ss foreach (msgAddElement(elem, msg, _))
        case OSCADDR_RE_HADD(elem) => ss foreach (msgAddElementToHeader(elem, msg, _))
        case _ =>
      }
    }
    //--
    println("r!OSC:" + addr + " [" + tt + "] " + ip + ":" + port)
  }


  // +++ TESTING
  def prepareDummyDataForTests (theView: score.Window) {
    // TESTING: adding dummy data
    for (i <- Iterator.range(0, 15))
      theView.addElement( new score.SimpleLine (
        (math.random * 30).toFloat,
        (math.random * 10).toFloat,
        Map("pch" -> (math.random * 10).toInt.toFloat, 
          "pchEnd" -> (math.random * 10).toInt.toFloat,
          "dyn" -> (math.random * 0.75f + 0.25f).toFloat )
        //Map[String, Float]
      ))
    theView.addElement( new score.Staff5 (1.f, 15.f, Map()))
    theView.addElement( new score.StaffReg3 (16.f, 25.f, Map()))

    theView.addHeaderElement( new score.Staff5 (0.f, 1.f, Map()))
  }

}

