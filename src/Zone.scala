/**
  * Created by Luke on 16/06/2016.
  */

//Zone class represents the different zones in a warehouse where stock is stored
//Zone name, distTOA, B, C, D, E, F, G, H, I, J
case class Zone (zoneName:String, distToA:Double, distToB:Double, distToC:Double, distToD:Double, distToE:Double,
                 distToF:Double, distToG:Double, distToH:Double, distToI:Double, distToJ:Double) {}

object Zone {
  def getDistToZone (zn:String): Unit = {

  }
}