/**
  * Created by Luke on 15/06/2016.
  */
case class OrderForm(orderID:String, customerID:String, firstName:String, surName:String, location:String, status:String, staffID:String){}

object OrderForm {
  var orders = Set(Product("01", "01", "tom", "Tommy", "Bristol", "Active", "01"))
  //possibly Seq instead
  def findByOrderId(orderId:String) = orderForms.find(_.orderID == orderId)

  def addOrder(productid: String, productname: String, quantity: Int): Unit = {
    orderForms = orderForms + new Product(productid, productname, quantity)
    orderList = orderList :+ new Product(productid, productname, quantity)
    orderCount += 1
  }

  def getOrder(index: Int): String = {
    orderList(index).productID + "," + orderList(index).productName + "," + orderList(index).quantity
  }
}

Product.findByOrderId(getUserInput())