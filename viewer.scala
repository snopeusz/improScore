package impro

import processing.core._
import PConstants._

import impro.spde._

import oscP5._
import netP5._

import impro._

object ScoreRunner {
  def main(args: Array[String]) {
    PApplet.main(args ++ Array("impro.Score") ) 
  }
}

class Score extends PApplet with Colors with Maths with Randoms {

  import PApplet._

  //lazy val oscP5 = new OscP5(this, 12000)
  //val myRemoteLocation = new NetAddress("127.0.0.1", 12000)
  //val myRemoteLocation = new NetAddress("127.0.0.1", 57120)

  val scoreView = new score.Window(this) 

  for (i <- Iterator.range(0, 5))
    scoreView.addElement( new score.SimpleNote (
      (math.random * 10).toFloat,
      (math.random * 10).toFloat,
      Map("pchStart" -> (math.random * 10).toFloat)
      //Map[String, Float]
    ))

  //scoreView.dumpVars

  var pos: Float = 0.f

  override def setup() {
    //size(1000, 700)
    size(700, 500)
    frameRate(24)
    smooth
    frame.setTitle("The Score")
    //oscP5; // właściwa inicjacja (lazy)
    scoreView.viewX = 30
    scoreView.viewY = 50

  }

  override def draw() {
    background(255)
    scoreView.pos = pos
    scoreView.draw

    pos += 0.1f
  }

  /*
  override def mousePressed() {
    val myMessage  = new OscMessage("/test")
    myMessage.add(123)
    oscP5.send(myMessage, myRemoteLocation)
  }


  def oscEvent(theOscMessage :OscMessage) {
    println("### received an osc message." + 
      " addrpattern: " + theOscMessage.addrPattern() + 
      " typetag: " + theOscMessage.typetag() )
    background(random(127))
  }
  */

}

