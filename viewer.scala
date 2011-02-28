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

  def oscEvent(theOscMessage :OscMessage) {
    println("### received an osc message."  
      + " addrpattern: " + theOscMessage.addrPattern 
      + " typetag: " + theOscMessage.typetag
      + " from: " + theOscMessage.address + ":" + theOscMessage.port
    )
    //background(random(127))
  }

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

