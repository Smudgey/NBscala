/**
  * Created by Luke on 15/06/2016.
  */

/**
  * Created by Administrator on 13/06/2016.
  */


import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object OrderDetails extends Enumeration {
  type OrderDetails = Value
  val ORDER_ID, NAME, LOCATION, ORDER_STATUS, STAFF_ID = Value
}

object WOTSMain {
  //Read in order data from csv and store in Array of Order
  def readInOrders(fileName:String): Array[Order] = {
    var orders:Array[Order] = Array.empty
    val orderDataSource = Source.fromFile(fileName)
    for (line <- orderDataSource.getLines) {
      val cols = line.split(",").map(_.trim) // Use comma to split each data value and trim excess spaces
      orders = orders :+ new Order(cols(0), cols(1), cols(2), cols(3), cols(4)) //append each order to array of orders
    }
    orderDataSource.close
    orders
  }

  //Read in staff data from csv and store in Array of Staff
  def readInStaff(fileName:String): Array[Staff] = {
    var staff:Array[Staff] = Array.empty
    val orderDataSource = Source.fromFile(fileName)
    for (line <- orderDataSource.getLines) {
      val cols = line.split(",").map(_.trim) // Use comma to split each data value and trim excess spaces
      staff = staff :+ new Staff(cols(0), cols(1)) //append each member of staff to array of staff
    }
    orderDataSource.close
    staff
  }

  //Write all order data to file
  def writeAllOrders(fileName:String, orders:Array[Order]): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    var text = ""
    for(i <- orders) {
      text = i.orderID + "," + i.name + "," + i.location + "," + i.status + "," + i.staffID + "\n"
      bw.write(text)
    }
    bw.close()
  }

  //Write all staff data to file
  def writeAllStaff(fileName:String, staff:Array[Staff]): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    var text = ""
    for(i <- staff) {
      text = i.staffID + "," + i.name + "\n"
      bw.write(text)
    }
    bw.close()
  }

  //Print a single order
  def printSingleOrder(orders:Array[Order], orderID:String): Unit = {
    var foundFlag = false
    println("Order ID \t Name \t Location \t Status \t Staff ID")
    for (i <- orders ) {
      if (orderID == i.orderID) {
        foundFlag = true
        println(i.orderID + "\t" + i.name + "\t" + i.location + "\t" + i.status + "\t" + i.staffID)
      }
    }
    if(!foundFlag)
      println("Order not found.\n")
  }

  //Print a single staff member details
  def printSingleStaff(staff:Array[Order], staffID:String): Unit = {
    var foundFlag = false
    println("Staff ID \t Name")
    for (i <- staff ) {
      if (staffID == i.staffID) {
        foundFlag = true
        println(i.staffID + "\t" + i.name)
      }
    }
    if(!foundFlag)
      println("Order not found.\n")
  }

  //Print all the orders stored in array
  def printOrders(orders:Array[Order]): Unit = {
    println("Order ID \t Name \t Location \t Status")
    for (i <- orders ) {
      println(i.orderID + "\t" + i.name + "\t" + i.location + "\t" + i.status + "\t" + i.staffID)
    }
  }

  //Print all the staff stored in array
  def printStaff(staff:Array[Staff]): Unit = {
    println("Staff ID \t Name")
    for (i <- staff ) {
      println(i.staffID + "\t" + i.name)
    }
  }

  def updateAnOrder (orders:Array[Order], orderID:String, updateType:OrderDetails.Value, userInput:String): Unit = {
    var foundFlag = false
    for(i <- orders ) {
      if (orderID == i.orderID) {
        i.status = "completed"
        updateType match {
          case OrderDetails.NAME =>
            //Update name
            i.name = userInput
            foundFlag = true
          case OrderDetails.LOCATION =>
            //Update location
            i.location = userInput
            foundFlag = true
          case OrderDetails.ORDER_STATUS =>
            //Update order Status
            i.status = userInput
            foundFlag = true
          case OrderDetails.STAFF_ID =>
            //Update ID of staff member working on the order
            i.staffID = userInput
            foundFlag = true
          case _ =>
            println("Invalid input")
        }
      }
    }
    if(!foundFlag)
      println("Order not found.\n")
  }

  //  def readAllOrders(fileName:String) {
  //    println("Order ID \t Name \t Location \t Status");
  //    val orderDataSource = Source.fromFile(fileName);
  //    for (line <- orderDataSource.getLines) {
  //      val cols = line.split(",").map(_.trim);
  //      println(s"${cols(0)} ${cols(1)} ${cols(2)} ${cols(3)}");
  //    }
  //    orderDataSource.close;
  //  }

//  def showOrderInformation(fileName:String, orderID:String) {
//    println("Order ID \t Name \t Location \t Status \t Staff ID")
//    val orderDataSource = Source.fromFile(fileName)
//    var foundFlag = false
//    for (line <- orderDataSource.getLines) {
//      val cols = line.split(",").map(_.trim)
//      if({cols(0)} == orderID) {
//        println(s"${cols(0)} ${cols(1)} ${cols(2)} ${cols(3)} ${cols(4)}")
//        foundFlag = true
//      }
//    }
//    if (!foundFlag)
//      println("No order found.")
//    orderDataSource.close
//  }

  def testUpdate(fileName:String, orderID:String) {
    println("Order ID \t Name \t Location \t Status \t Staff ID")
    val orderDataSource = Source.fromFile(fileName)
    var foundFlag = false
    for (line <- orderDataSource.getLines) {
      val cols = line.split(",").map(_.trim)
      if({cols(0)} == orderID) {
        println(s"${cols(0)} ${cols(1)} ${cols(2)} ${cols(3)}")
        cols(3) = "completed"
        println(s"${cols(0)} ${cols(1)} ${cols(2)} ${cols(3)} ${cols(4)}")
        //val writer = new CSVWriter(out);
        foundFlag = true
      }
    }

    if (!foundFlag)
      println("No order found.")

    orderDataSource.close
  }

  def getUserInput(): String = {
    println("Please enter a value: ")
    scala.io.StdIn.readLine()
  }

  def main(args: Array[String]) {
    var menuFlag = true
    var userInput1 = "0"
    val orderFName = "orders.csv"
    val staffFName = "staff.csv"

    //Read in and store data
    val orders:Array[Order] = readInOrders(orderFName)
    val staff:Array[Staff] = readInStaff(staffFName)

    //updateAnOrder(orders, "003");
    //writeAllOrders(orderFName, orders);

    //testUpdate("orders.csv", "003");
    //menuFlag = false;

    while(menuFlag){
      println("1. View all orders")
      println("2. View an order")
      println("3. Update an order")

      getUserInput() match {
        case "1" => // View
          println(" ")
          printOrders(orders)
        case "2" =>
          println("What is the order ID? ")
          printSingleOrder(orders, getUserInput())
        case "3" =>
          println("What is the order ID? ")
          userInput1 = getUserInput()
          printSingleOrder(orders, userInput1)

          //Ask user what they want to do
          println("What would you like to update?")
          println("1. Name")
          println("2. Location")
          println("3. Status")
          println("4. Staff ID")

          getUserInput() match {
            case "1" =>
              //Name
              updateAnOrder(orders, userInput1, OrderDetails.NAME, getUserInput())
            case "2" =>
              //Location
              updateAnOrder(orders, userInput1, OrderDetails.LOCATION, getUserInput())
            case "3" =>
              //Status
              updateAnOrder(orders, userInput1, OrderDetails.ORDER_STATUS, getUserInput())
            case "4" =>
              //Staff ID
              updateAnOrder(orders, userInput1, OrderDetails.STAFF_ID, getUserInput())
            case _ =>
              //Invalid
              println("Invalid option.")
          }
          writeAllOrders(orderFName, orders)
        case _ => menuFlag = false
      }
    }
  }
}
