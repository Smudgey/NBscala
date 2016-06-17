/**
  * Created by Luke on 15/06/2016.
  */
class OrderForm(oID:String, cID:String, fn:String, sn:String, loc:String, stat:String, sid:String){
  var orderID:String = oID
  var customerID:String = cID
  var firstName:String = fn
  var surname:String = sn
  var location:String = loc
  var status:String = stat
  var staffID:String = sid
  var orderList:Array[Order] = Array.empty
  var orderCount = 0

  def addOrder(productid:String, productname:String, quantity:Int): Unit = {
    orderList = orderList :+ new Order(productid, productname, quantity)
    orderCount += 1
  }

  def getOrder(index:Int): String = {
    orderList(index).productID + "," + orderList(index).productName + "," + orderList(index).quantity
  }
}