/**
  * Created by Luke on 17/06/2016.
  */

import java.sql.DriverManager
import java.sql.Connection

object DatabaseTest {
  def main(args: Array[String]) {
    // connect to the database named "mysql" on the localhost
    val driver = "com.mysql.jdbc.Driver"
    val url = "jdbc:mysql://localhost/nbtest1"
    val username = "root"
    val password = "admin"

    // there's probably a better way to do this
    var connection:Connection = null

    try {
      // make the connection
      Class.forName(driver)
      connection = DriverManager.getConnection(url, username, password)

      // create the statement, and run the select query
      val statement = connection.createStatement()
      val resultSet = statement.executeQuery("SELECT * FROM staff")
      while ( resultSet.next() ) {
        val staffid = resultSet.getString("StaffID")
        val firstname = resultSet.getString("FirstName")
        val surname = resultSet.getString("Surname")
        println("staffID, Name = " + staffid + ", " + firstname + " " + surname)
      }
    } catch {
      case e => e.printStackTrace
    }
    connection.close()
  }
}