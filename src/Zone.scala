/**
  * Created by Luke on 16/06/2016.
  */

//Zone class represents the different zones in a warehouse where stock is stored
//Zone name, distTOA, B, C, D, E, F, G, H, I, J
class Zone (zn:String, a:Double, b:Double, c:Double, d:Double, e:Double,
            f:Double, g:Double, h:Double, i:Double, j:Double) {
  var zoneName:String = zn
  var distToA:Double = a
  var distToB:Double = b
  var distToC:Double = c
  var distToD:Double = d
  var distToE:Double = e
  var distToF:Double = f
  var distToG:Double = g
  var distToH:Double = h
  var distToI:Double = i
  var distToJ:Double = j
  var excluded:Boolean = false

  def setExclusion(f:Boolean) : Unit = {
    excluded = f
  }
}
