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
    PApplet.main(args  ++ Array("impro.Score") ) 
  }
}

class Score extends PApplet with Colors with Maths with Randoms {

  import PApplet._


  lazy val oscP5 = new OscP5(this, 12000)
  //val myRemoteLocation = new NetAddress("127.0.0.1", 12000)
  val myRemoteLocation = new NetAddress("127.0.0.1", 57120)

  //val scoreView = new score.Window(this, 1280, 500, 0.1f) 
  lazy val scoreView = new score.Window(this, width, height, 0.1f) 

  lazy val fontReg = loadFont("lib/TeXGyreTermes-Regular-48.vlw")

  //scoreView.dumpVars

  var pos: Float = 0.f


  override def setup() {
    size(1280, 500)
    //size(920, 420)
    //size(1000, 800)
    //size(700, 500)
    frameRate(30)
    smooth
    frame.setTitle("The Score")
    oscP5 // właściwa inicjacja (lazy)
    scoreView.viewX = 0
    scoreView.viewY = 0
    prepareDummyDataForTests
    //fontReg
    textFont(fontReg)
  }

  override def draw() {
    background(255)
    scoreView.pos = pos
    scoreView.draw

    pos += 0.03f

    pushStyle
    fill(0)
    text(frameRate.toString take 6, 10, 150);
    fill(55,0,0)
    text(pos.toString take 6, 10, 100);
    fill(0,55,0)
    text(scoreView.elementsNumber.toString, 10, 50);
    popStyle
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

  private def msgSync (msg: OscMessage, theView: score.Window) {
    println("syncing!")
  }

  private def msgSetPos (msg: OscMessage, theView: score.Window) {
    println("set pos!")
    if (msg.typetag startsWith "if") {
      pos = msg.get(1).floatValue
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
    // TODO
  }
  private def msgSetPosWindow (msg: OscMessage, theView: score.Window) {
    println("set posWindow!")
    // TODO
  }
  private def msgSetPosOffset (msg: OscMessage, theView: score.Window) {
    println("set posOffset!")
    // TODO
  }
  private def msgSetSpeed (msg: OscMessage, theView: score.Window) {
    println("set speed!")
    // TODO
  }

  // this should be in scoreElement.scala, shouldn't it ?
  val names2elements = Map(
    "simpleLine"    -> score.SimpleLine,
    "simpleNote"    -> score.SimpleNote,
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

  // *** MAIN OSC RESPONDER:
  def oscEvent(msg :OscMessage) {
    val addr = msg.addrPattern
    val tt = msg.typetag
    val ip = msg.address
    val port = msg.port
    val theView = scoreView

    if (addr.startsWith("/imps") && tt.startsWith("i"))
    addr match {
      case OSCADDR_SYNC => msgSync(msg, theView)
      case OSCADDR_SETPOS => msgSetPos(msg, theView)
      case OSCADDR_CLEAR => msgClear(msg, theView)
      case OSCADDR_HCLEAR => msgClearHeader(msg, theView)
      case OSCADDR_SETTIMEWIN => msgSetTimeWindow(msg, theView)
      case OSCADDR_SETPOSWIN => msgSetPosWindow(msg, theView)
      case OSCADDR_SETPOSOFFSET => msgSetPosOffset(msg, theView)
      case OSCADDR_SETSPEED => msgSetSpeed(msg, theView)
      case OSCADDR_RE_ADD(elem) => msgAddElement(elem, msg, theView)
      case OSCADDR_RE_HADD(elem) => msgAddElementToHeader(elem, msg, theView)
      case _ =>
    }
    //--
    println("r!OSC:" + addr + " [" + tt + "] " + ip + ":" + port)
  }


  // +++ TESTING
  def prepareDummyDataForTests {
    // TESTING: adding dummy data
    for (i <- Iterator.range(0, 15))
      scoreView.addElement( new score.SimpleLine (
        (math.random * 30).toFloat,
        (math.random * 10).toFloat,
        Map("pch" -> (math.random * 10).toInt.toFloat, 
          "pchEnd" -> (math.random * 10).toInt.toFloat,
          "dyn" -> (math.random * 0.75f + 0.25f).toFloat )
        //Map[String, Float]
      ))
    scoreView.addElement( new score.Staff5 (1.f, 15.f, Map()))
    scoreView.addElement( new score.StaffReg3 (16.f, 25.f, Map()))

    scoreView.addHeaderElement( new score.Staff5 (0.f, 1.f, Map()))
  }

}

