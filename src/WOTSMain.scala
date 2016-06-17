/**
  * Created by Luke on 15/06/2016.
  */

/**
  * Created by Administrator on 13/06/2016.
  */

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object FileNames extends Enumeration {
  type FileNames = Value
  val ORDER_FILE = "orderForms.csv"
  val STAFF_FILE = "staff.csv"
  val STOCK_FILE = "stock.csv"
}

object OrderFormDetails extends Enumeration {
  type OrderFormDetails = Value
  val ORDER_ID, FIRST_NAME, SURNAME, LOCATION, ORDER_STATUS, STAFF_ID = Value
}

object StaffDetails extends Enumeration {
  type StaffDetails = Value
  val STAFF_ID, FIRST_NAME, SURNAME = Value
}

object StockDetails extends Enumeration {
  type StockDetails = Value
  val STOCK_ID, NAME, QUANTITY, ZONE = Value
}

object WOTSMain {
  //Read in order data from csv and store in Array of Order
  def readInOrders(): Array[OrderForm] = {
    var orders:Array[OrderForm] = Array.empty
    val orderDataSource = Source.fromFile(FileNames.ORDER_FILE)
    var flag = true
    var newOrderForm:OrderForm = new OrderForm("", "", "", "", "", "", "")
    for (line <- orderDataSource.getLines) { //grab each line in csv
      //col(7) = order 1 id, col(8) = order 1 name, col(9) = order 1 quantity
      val cols = line.split(",").map(_.trim) // Use comma to split each data value and trim excess spaces
      var ifEmptyCheck:Int = 10
      var orderIDIndex = 7
      var orderNameIndex = 8
      var orderQuantityIndex = 9
      flag = true

      //create a new Order Form
      newOrderForm = new OrderForm(cols(0), cols(1), cols(2), cols(3), cols(4), cols(5),
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
    orderDataSource.close
    orders
  }

  //Read in staff data from csv and store in Array of Staff
  def readInStaff(): Array[Staff] = {
    var staff:Array[Staff] = Array.empty
    val orderDataSource = Source.fromFile(FileNames.STAFF_FILE)
    for (line <- orderDataSource.getLines) {
      val cols = line.split(",").map(_.trim) // Use comma to split each data value and trim excess spaces
      staff = staff :+ new Staff(cols(0), cols(1), cols(2)) //append each member of staff to array of staff
    }
    orderDataSource.close
    staff
  }

  //Read in stock data from csv and store in Array of Stock
  def readInStock(): Array[Stock] = {
    var stock:Array[Stock] = Array.empty
    val orderDataSource = Source.fromFile(FileNames.STOCK_FILE)
    for (line <- orderDataSource.getLines) {
      val cols = line.split(",").map(_.trim) // Use comma to split each data value and trim excess spaces
      stock = stock :+ new Stock(cols(0), cols(1), cols(2), cols(3)) //append stock info
    }
    orderDataSource.close
    stock
  }

  //Write all order data to file
  def writeAllOrders(orders:Array[OrderForm]): Unit = {
    val file = new File(FileNames.ORDER_FILE)
    val bw = new BufferedWriter(new FileWriter(file))
    var text = ""
    for(i <- orders) {
      text = i.orderID + "," + i.customerID + "," + i.firstName + "," + i.surname + "," +
        i.location + "," + i.status + "," + i.staffID
      var currentIndex = 0
      var flag = true

      while (flag) { //while there is still orders to write
        text += "," + i.getOrder(currentIndex)
        currentIndex += 1

        //check if any orders left
        if ( i.orderCount == currentIndex)
          flag = false
      }
      text += "\n"
      bw.write(text)
    }
    bw.close()
  }

  //Write all staff data to file
  def writeAllStaff(staff:Array[Staff]): Unit = {
    val file = new File(FileNames.STAFF_FILE)
    val bw = new BufferedWriter(new FileWriter(file))
    var text = ""
    for(i <- staff) {
      text = i.staffID + "," + i.firstName + "," + i.surname + "\n"
      bw.write(text)
    }
    bw.close()
  }

  //Write all stock data to file
  def writeAllStock(stock:Array[Stock]): Unit = {
    val file = new File(FileNames.STOCK_FILE)
    val bw = new BufferedWriter(new FileWriter(file))
    var text = ""
    for(i <- stock) {
      text = i.stockID + "," + i.name + "," + i.quantity + "," + i.zone + "\n"
      bw.write(text)
    }
    bw.close()
  }

  //Print a single order TODO
  def printSingleOrder(orders:Array[OrderForm], orderID:String): Unit = {
    var foundFlag = false
    println("Order ID \t Name \t Location \t Status \t Staff ID \t Product ID, Product Name, Product Quantity")
    for (i <- orders ) {
      if (orderID == i.orderID) {
        foundFlag = true
        println(i.orderID + "\t" + i.firstName + " " + i.surname + "\t" + i.location + "\t" + i.status + "\t" + i.staffID)

        //Loop each order and print
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
        println(i.staffID + "\t" + i.firstName + " " + i.surname)
      }
    }
    if(!foundFlag)
      println("Staff not found.\n")
  }

  //Print info on one piece of stock
  def printSingleStock(stock:Array[Stock], stockID:String): Unit = {
    var foundFlag = false
    println("Stock ID \t Name \t Quantity")
    for (i <- stock ) {
      if (stockID == i.stockID) {
        foundFlag = true
        println(i.stockID + "\t" + i.name + "\t" + i.quantity)
      }
    }
    if(!foundFlag)
      println("Stock not found.\n")
  }

  //Print all the orders stored in array
  def printOrders(orders:Array[OrderForm]): Unit = {
    println("Order ID \t Name \t Location \t Status \t Staff ID")
    for (i <- orders ) {
      println(i.orderID + "\t" + i.firstName + " " + i.surname + "\t" + i.location + "\t" + i.status + "\t" + i.staffID)
    }
  }

  //Print all the staff stored in array
  def printStaff(staff:Array[Staff]): Unit = {
    println("Staff ID \t Name")
    for (i <- staff ) {
      println(i.staffID + "\t" + i.firstName + " " + i.surname)
    }
  }

  //Print all stock
  def printStock(stock:Array[Stock]): Unit = {
    println("Stock ID \t Name \t Quantity \t Zone")
    for (i <- stock ) {
      println(i.stockID + "\t" + i.name + "\t" + i.quantity + "\t" + i.zone)
    }
  }

  //Add a new order TODO
  def addOrder(): OrderForm = {
    //Requires id, name, location, status, staffID
    println("Please enter the details for the new order.")
    println("Order ID")
    val orderID = getUserInput()
    println("Customer ID")
    val custID = getUserInput()
    println("First Name")
    val fname = getUserInput()
    println("Surname")
    val sname = getUserInput()
    println("Location")
    val location = getUserInput()
    println("Status")
    val status = getUserInput()
    println("staffID")
    val staffID = getUserInput()

    new OrderForm(orderID, custID, fname, sname, location, status, staffID)
  }

  //Add a new member of staff
  def addStaff (): Staff = {
    //Requires id, name
    println("Please enter the details for the new staff member.")
    println("Staff ID")
    val staffID = getUserInput()
    println("First Name")
    val fname = getUserInput()
    println("Surname")
    val sname = getUserInput()

    new Staff(staffID, fname, sname)
  }

  //Add new stock info
  def addStock (): Stock = {
    //Requires id, name
    println("Please enter the details for the new stock.")
    println("Stock ID")
    val stockID = getUserInput()
    println("Name")
    val name = getUserInput()
    println("Quantity")
    val quantity = getUserInput()
    println("Zone")
    val zone = getUserInput()

    new Stock(stockID, name, quantity, zone)
  }

  //Updates a specific order, found with orderID TODO
  def updateOrder(orders:Array[OrderForm], orderID:String, updateType:OrderFormDetails.Value, userInput:String): Unit = {
    var foundFlag = false
    for(i <- orders ) {
      if (orderID == i.orderID) {
        updateType match {
          case OrderFormDetails.FIRST_NAME =>
            //Update first name
            i.firstName = userInput
            foundFlag = true
          case OrderFormDetails.SURNAME =>
            //Update surname
            i.surname = userInput
            foundFlag = true
          case OrderFormDetails.LOCATION =>
            //Update location
            i.location = userInput
            foundFlag = true
          case OrderFormDetails.ORDER_STATUS =>
            //Update order Status
            i.status = userInput
            foundFlag = true
          case OrderFormDetails.STAFF_ID =>
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
  def updateStaff(staff:Array[Staff], staffID:String, updateType:StaffDetails.Value, userInput:String): Unit = {
    var foundFlag = false
    for(i <- staff ) {
      if (staffID == i.staffID) {
        updateType match {
          case StaffDetails.FIRST_NAME =>
            //Update first name
            i.firstName = userInput
            foundFlag = true
          case StaffDetails.SURNAME =>
            //Update surname
            i.surname = userInput
            foundFlag = true
          case _ =>
            println("Invalid input")
        }
      }
    }
    if(!foundFlag)
      println("Staff not found.\n")
  }

  //Updates a specific stock, found with stockID
  def updateStock(stock:Array[Stock], stockID:String, updateType:StockDetails.Value, userInput:String): Unit = {
    var foundFlag = false
    for(i <- stock ) {
      if (stockID == i.stockID) {
        updateType match {
          case StockDetails.NAME =>
            //Update name
            i.name = userInput
            foundFlag = true
          case StockDetails.QUANTITY =>
            //Update stock
            i.quantity = userInput
            foundFlag = true
          case StockDetails.ZONE =>
            //Update zone
            i.zone = userInput
            foundFlag = true
          case _ =>
            println("Invalid input")
        }
      }
    }
    if(!foundFlag)
      println("Stock not found.\n")
  }

  def getUserInput(): String = {
    println("Please enter a value: ")
    scala.io.StdIn.readLine()
  }

  //perform a 'greedy' solution to the TSP, immediately choosing the next closest zone
  def greedySalesmanAlg(): Unit = {

  }

  //perform a 'brute force' solution to the TSP, checking every possible combination until the optimal route is found
  def bruteForceSalesmanAlg(): Unit = {

  }

  def main(args: Array[String]) {
    var menuFlag = true
    var userInput1 = "0"

    //Read in and store data
    var orders:Array[OrderForm] = readInOrders()
    var staff:Array[Staff] = readInStaff()
    var stock:Array[Stock] = readInStock()

    while(menuFlag){
      println("\nWelcome to the Warehouse Order Tracking System.")
      println("To navigate, simply enter the number of the service you wish to access.")
      println("Enter 0 to exit")
      println("1. View all orders")//done
      println("2. Find an order")//done
      println("3. Update an order")//done
      println("4. Add an order")//done
      println("5. View all staff")//done
      println("6. Find a staff member")//done
      println("7. Update staff details")//done
      println("8. Add staff")//done
      println("9. View all stock")//done
      println("10. Find specific stock")//done
      println("11. Update stock")//done
      println("12. Add stock")//done

      getUserInput() match {
        case "1" =>
          // View orders
          println(" ")
          printOrders(orders)
        case "2" =>
          //Find an order
          println("What is the order ID? ")
          printSingleOrder(orders, getUserInput())
        case "3" => //TODO
          //Update an order
          println("What is the order ID? ")
          userInput1 = getUserInput()
          printSingleOrder(orders, userInput1)

          //Ask user what they want to do
          println("What would you like to update?")
          println("1. First Name")
          println("2. Surname")
          println("3. Location")
          println("4. Status")
          println("5. Staff ID")

          getUserInput() match {
            case "1" =>
              //First Name
              updateOrder(orders, userInput1, OrderFormDetails.FIRST_NAME, getUserInput())
            case "2" =>
              //Surname
              updateOrder(orders, userInput1, OrderFormDetails.SURNAME, getUserInput())
            case "3" =>
              //Location
              updateOrder(orders, userInput1, OrderFormDetails.LOCATION, getUserInput())
            case "4" =>
              //Status
              updateOrder(orders, userInput1, OrderFormDetails.ORDER_STATUS, getUserInput())
            case "5" =>
              //Staff ID
              updateOrder(orders, userInput1, OrderFormDetails.STAFF_ID, getUserInput())
            case _ =>
              //Invalid
              println("Invalid option.")
          }
          writeAllOrders(orders)
        case "4" =>
          //Add an order
          orders = orders :+ addOrder()
          writeAllOrders(orders)
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
          println("1. First Name")
          println("2. Surname")

          getUserInput() match {
            case "1" =>
              //First Name
              updateStaff(staff, userInput1, StaffDetails.FIRST_NAME, getUserInput())
            case "2" =>
              //Surname
              updateStaff(staff, userInput1, StaffDetails.SURNAME, getUserInput())
            case _ =>
              //Invalid input
              println("Invalid Input.")
          }
          writeAllStaff(staff)
        case "8" =>
          //add a new staff member
          staff = staff :+ addStaff()
          writeAllStaff(staff)
        case "9" =>
          //View all stock
          println(" ")
          printStock(stock)
        case "10" =>
          //Find specific stock
          println("What is the stock ID? ")
          printSingleStock(stock, getUserInput())
        case "11" =>
          //Update stock
          println("What is the stock ID?")
          userInput1 = getUserInput()
          printSingleStock(stock, userInput1)

          //Ask user which field they wish to update
          println("What would you like to update?")
          println("1. Name")
          println("2. Quantity")
          println("3. Zone")

          getUserInput() match {
            case "1" =>
              //Name
              updateStock(stock, userInput1, StockDetails.NAME, getUserInput())
            case "2" =>
              //Quantity
              updateStock(stock, userInput1, StockDetails.QUANTITY, getUserInput())
            case "3" =>
              //Zone
              updateStock(stock, userInput1, StockDetails.ZONE, getUserInput())
            case _ =>
              //Invalid input
              println("Invalid Input.")
          }
          writeAllStock(stock)
        case "12" =>
          //Add new stock
          stock = stock :+ addStock()
          writeAllStock(stock)
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
