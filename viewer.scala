package impro

import processing.core._
import PConstants._

import impro.spde._

import oscP5._
import netP5._

import impro.element._

object ScoreRunner {
  def main(args: Array[String]) {
    PApplet.main(args ++ Array("impro.Score") ) 
  }
}

class Score extends PApplet with Colors with Maths with Randoms {

  import PApplet._

  lazy val oscP5 = new OscP5(this, 12000)
  //val myRemoteLocation = new NetAddress("127.0.0.1", 12000)
  val myRemoteLocation = new NetAddress("127.0.0.1", 57120)

  val seList = List(
    new ScoreElement(7,1,20,40), 
    new ScoreElement(2,8,10,7),
    new ScoreElement(0,1,20,40), 
    new ScoreElement(1,2,80,70),
    new ScoreElement(4,2,50,50)
  )

  val xscale :Float = 100.0f
  val yscale :Float = 300.0f

  var pos :Float = 0.f

  override def setup() {
    size(1000, 700)
    frameRate(24)
    smooth
    frame.setTitle("The Score")
    //noLoop
    //println(oscP5)
    oscP5; // właściwa inicjacja (lazy)

  }

  override def draw() {
    //background(Mod255(mouseX))
    //background(mouseX%255)
    background(255)
    for (se <- seList) {
      //se.drawAt(this, se.beg * xscale, 100)
      //image(se.img, se.beg * xscale, 100)
      se.drawAt(this, (se.beg * xscale) - pos, 150)
    }
  
    pos = pos + 0.8
  }

  override def mousePressed() {
    val myMessage  = new OscMessage("/test")
    myMessage.add(123)
    oscP5.send(myMessage, myRemoteLocation)
  }


  def oscEvent(theOscMessage :OscMessage) {
    /* print the address pattern and the typetag of the received OscMessage */
    println("### received an osc message." + 
      " addrpattern: " + theOscMessage.addrPattern() + 
      " typetag: " + theOscMessage.typetag() )
    background(random(127))
  }

}

