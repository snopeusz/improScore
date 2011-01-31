package impro

import processing.core._
//import spde.core._
import impro.spde._
import PConstants._
import PApplet._
//import impro.calc._

import oscP5._
import netP5._

/*
TODO: czy w ogóle spde.core jest potrzebne?
Można wyjąć tylko przydatne rzeczy z Enriched.scala
dołączyć do tego projektu i uniknąć zależności od 
kilku innych bibliotek (dispatcher-cośtam...)
*/

object ScoreRunner {
  def main(args: Array[String]) {
    //println(args)
    /* argumenty przed nazwą klasy apletu! */
    PApplet.main(args ++ Array("impro.Score") ) 
    //Applet.main(Array(classOf[Score].getName)) 
  }
}

class Score extends PApplet with Colors with Maths with Randoms {
//class Score extends ProxiedApplet {
  //lazy val px = new DrawProxy(this) {

  override def setup() {
    size(400, 300)
    frame.setTitle("The Score")
    //noLoop
  }

  override def draw() {
    background(Mod255(mouseX))
    //background(mouseX%255)

  }

//}
}

