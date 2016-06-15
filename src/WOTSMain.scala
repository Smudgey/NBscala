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

object StaffDetails extends Enumeration {
  type StaffDetails = Value
  val STAFF_ID, NAME = Value
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
  def printSingleStaff(staff:Array[Staff], staffID:String): Unit = {
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

  //Add a new order
  def addAnOrder (fileName:String): Order = {
    //Requires id, name, location, status, staffID
    println("Please enter the details for the new order.")
    println("Order ID")
    val orderID = getUserInput()
    println("Name")
    val name = getUserInput()
    println("Location")
    val location = getUserInput()
    println("Status")
    val status = getUserInput()
    println("staffID")
    val staffID = getUserInput()

    new Order(orderID, name, location, status, staffID)
  }

  //Add a new member of staff
  def addStaff (fileName:String): Staff = {
    //Requires id, name
    println("Please enter the details for the new staff member.")
    println("Staff ID")
    val staffID = getUserInput()
    println("Name")
    val name = getUserInput()
    new Staff(staffID, name)
  }

  //Updates a specific order, found with orderID
  def updateAnOrder (orders:Array[Order], orderID:String, updateType:OrderDetails.Value, userInput:String): Unit = {
    var foundFlag = false
    for(i <- orders ) {
      if (orderID == i.orderID) {
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

  //Updates a specific staff members details, found with staffID
  def updateAStaff (staff:Array[Staff], staffID:String, updateType:StaffDetails.Value, userInput:String): Unit = {
    var foundFlag = false
    for(i <- staff ) {
      if (staffID == i.staffID) {
        updateType match {
          case StaffDetails.NAME =>
            //Update name
            i.name = userInput
            foundFlag = true
          case _ =>
            println("Invalid input")
        }
      }
    }
    if(!foundFlag)
      println("Staff not found.\n")
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

//  def testUpdate(fileName:String, orderID:String) {
//    println("Order ID \t Name \t Location \t Status \t Staff ID")
//    val orderDataSource = Source.fromFile(fileName)
//    var foundFlag = false
//    for (line <- orderDataSource.getLines) {
//      val cols = line.split(",").map(_.trim)
//      if({cols(0)} == orderID) {
//        println(s"${cols(0)} ${cols(1)} ${cols(2)} ${cols(3)}")
//        cols(3) = "completed"
//        println(s"${cols(0)} ${cols(1)} ${cols(2)} ${cols(3)} ${cols(4)}")
//        //val writer = new CSVWriter(out);
//        foundFlag = true
//      }
//    }
//
//    if (!foundFlag)
//      println("No order found.")
//
//    orderDataSource.close
//  }

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
    var orders:Array[Order] = readInOrders(orderFName)
    var staff:Array[Staff] = readInStaff(staffFName)

    while(menuFlag){
      println("\nWelcome to the Warehouse Order Tracking System.")
      println("To navigate, simply enter the number of the service you wish to access.")
      println("Enter 0 to exit")
      println("1. View all orders")//done
      println("2. Find an order")//done
      println("3. Update an order")//done
      println("4. Add an order")
      println("5. View all staff")//done
      println("6. Find a staff member")//done
      println("7. Update staff details")//done
      println("8. Add staff")
      println("9. View all stock")
      println("10. Find specific stock")
      println("11. Update stock")

      getUserInput() match {
        case "1" =>
          // View orders
          println(" ")
          printOrders(orders)
        case "2" =>
          //Find an order
          println("What is the order ID? ")
          printSingleOrder(orders, getUserInput())
        case "3" =>
          //Update an order
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
        case "4" =>
          //Add an order
          orders = orders :+ addAnOrder(orderFName)
          writeAllOrders(orderFName, orders)
        case "5" =>
          //View all staff
          println(" ")
          printStaff(staff)
        case "6" =>
          //Find a single staff member
          println("What is the staff ID? ")
          printSingleStaff(staff, getUserInput())
        case "7" =>
          //Update staff details
          println("What is the staff ID?")
          userInput1 = getUserInput()
          printSingleStaff(staff, userInput1)

          //Ask user which field they wish to update
          println("What would you like to update?")
          println("1. Name")

          getUserInput() match {
            case "1" =>
              //Name
              updateAStaff(staff, userInput1, StaffDetails.NAME, getUserInput())
            case _ =>
              //Invalid input
              println("Invalid Input.")
          }
          writeAllStaff(staffFName, staff)
        case "8" =>
          //add a new staff member
          staff = staff :+ addStaff(staffFName)
          writeAllStaff(staffFName, staff)
        case "9" =>
          //View all stock
        case "10" =>
          //Find specific stock
        case "11" =>
          //Update stock
        case "0" =>
          //Exit program
          menuFlag = false;
        case _ =>
          //Invalid input
          println("Invalid input")
      }
    }
  }
}
