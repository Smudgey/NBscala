/**
  * Created by Luke on 15/06/2016.
  */
case class Order(orderID:String, customerID:String, firstName:String, surName:String, location:String, status:String, staffID:String){}

object Order {
  var orderLine = Set
  //Set(OrderLine("01", "01", "tom", "Tommy", "Bristol", "Active", "01"))
  //possibly Seq instead
  def findByOrderId(orderId:String) = orders.find(_.orderID == orderId) {}

  def addOrder(productid: String, productname: String, quantity: Int): Unit = {
    orderForms = orderForms + new Product(productid, productname, quantity)
    orderLine = orderLine :+ new Product(productid, productname, quantity)
  }

  def getOrder(index: Int): String = {
    orderList(index).productID + "," + orderList(index).productName + "," + orderList(index).quantity
  }

  def readInOrders(): Set = {
    var flag = true
    var newOrderForm:Order = new Order("", "", "", "", "", "", "")
    for (line <- orderDataSource.getLines) { //grab each line in csv
    //col(7) = order 1 id, col(8) = order 1 name, col(9) = order 1 quantity
    val cols = line.split(",").map(_.trim) // Use comma to split each data value and trim excess spaces
    var ifEmptyCheck:Int = 10
      var orderIDIndex = 7
      var orderNameIndex = 8
      var orderQuantityIndex = 9
      flag = true

      //create a new Order Form
      newOrderForm = new Order(cols(0), cols(1), cols(2), cols(3), cols(4), cols(5),
        cols(6))

      while(flag) { //while the orders have not all been added
        //Add order to form
        newOrderForm.addOrder(cols(orderIDIndex), cols(orderNameIndex), cols(orderQuantityIndex).toInt)

        if (cols.size <= ifEmptyCheck) {
          flag = !flag
        } else {
          orderIDIndex += 3
          orderNameIndex += 3
          orderQuantityIndex += 3
          ifEmptyCheck +=3
        }
      }
      //append each order to array of orders
      orders = orders :+ newOrderForm
    }
  }
}

//Product.findByOrderId(getUserInput())