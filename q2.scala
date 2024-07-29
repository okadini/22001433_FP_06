import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

object q2 {
    def main(args:Array[String]) = {

        def validateInput(name:String,marks:Int,totalMarks:Int):(Boolean,Option[String]) = {

            var isValid:Boolean = true
            var errorMessage:Option[String] = None

            if(name == "") {
                isValid = false
                errorMessage = Some("N")
            }

            if(marks < 0 || marks > totalMarks) {
                isValid = false
                errorMessage = Some("M")
            }

            if(totalMarks <= 0) {
                isValid = false
                errorMessage = Some("T")
            }

            if(name == "" && (marks < 0 || marks > totalMarks)) {
                isValid = false 
                errorMessage = Some("N")
            }

            if(name == "" && totalMarks <= 0) {
                isValid = false
                errorMessage = Some("NT")
            }

            if((marks < 0 || marks > totalMarks) && totalMarks <= 0) {
                isValid = false
                errorMessage = Some("MT")
            }

            if(name == "" && (marks < 0 || marks > totalMarks) && totalMarks <= 0) {
                isValid = false
                errorMessage = Some("NMT")
            }

            (isValid,errorMessage)
        }

        def getStudentInfoWithRetry(isValid:Boolean,errorMessage:Option[String],name:String,marks:Int,totalMarks:Int) = {

            var newName = name
            var newMarks = marks
            var newTotalMarks = totalMarks
            var newErrorMessage = errorMessage
            var newIsValid = isValid
            var percentage = 0.0
            var grade = 'E'

            while(!newIsValid) {

                newErrorMessage match {
                    case Some("N") =>
                        println("Invalid name\n")
                        newName = readLine("Re-enter the name : ")
                
                    case Some("M") =>
                        println("Invalid marks\n")
                        print("Re-nter the marks : ")
                        newMarks = readInt()

                    case Some("T") =>
                        println("Invalid possible totalMarks\n")
                        print("Re-enter the possible total marks : ")
                        newTotalMarks = readInt()
                
                    case Some("NM") =>
                        println("Invalid name and marks\n")
                        newName = readLine("Re-enter the name : ")
                        print("Re-enter the marks : ")
                        newMarks = readInt()

                    case Some("NT") =>
                        println("Invalid name and possible total marks\n")
                        newName = readLine("Re-enter the name : ")
                        print("Re-enter the possible total marks : ")
                        newTotalMarks = readInt()
        
                    case Some("MT") =>
                        println("Invalid marks and possible total marks\n")
                        print("Re-enter the marks : ")
                        newMarks = readInt()
                        print("Re-enter the possible total marks : ")
                        newTotalMarks = readInt()

                    case Some("NMT") =>
                        println("Invalid name,marks and possible total marks\n")
                        newName = readLine("Re-enter the name : ")
                        print("Re-enter the marks : ")
                        newMarks = readInt()
                        print("Re-enter the possible total marks : ")
                        newTotalMarks = readInt()
                    
                    case _ => println("Unknown error")
                }

                var (isValidAfterUpdate,errorMessageAfterUpdate) = validateInput(newName,newMarks,newTotalMarks) 

                newIsValid = isValidAfterUpdate
                newErrorMessage = errorMessageAfterUpdate
            }
            
            percentage = (newMarks.toDouble / newTotalMarks) * 100

            grade = 'E'

            if(percentage >= 90) {
                grade = 'A'
            }
            else if(percentage < 90 && percentage >= 75) {
                grade = 'B'
            }
            else if(percentage < 75 && percentage >=50) {
                grade = 'C'
            }
            else {
                grade = 'D'
            }

            (newName,newMarks,newTotalMarks,percentage,grade)
        }

        def getStudentInfo() : (String,Int,Int,Double,Char) = {

            var name = ""
            var marks = 0
            var totalMarks = 0
            var percentage = 0.0
            var grade = 'E' 

            name = readLine("Enter the name : ")
            print("Enter the marks : ")
            marks = readInt()
            print("Enter the possible total marks : ")
            totalMarks = readInt()

            val (isValid,errorMessage) = validateInput(name,marks,totalMarks) 

            if(isValid) {

                percentage = (marks.toDouble / totalMarks) * 100

                if(percentage >= 90) {
                    grade = 'A'
                }
                else if(percentage < 90 && percentage >= 75) {
                    grade = 'B'
                }
                else if(percentage < 75 && percentage >=50) {
                    grade = 'C'
                }
                else {
                    grade = 'D'
                }

                (name,marks,totalMarks,percentage,grade)
            }

            else {
                getStudentInfoWithRetry(isValid,errorMessage,name,marks,totalMarks)
            }
        }

        def printStudentRecord(record:Option[(String,Int,Int,Double,Char)]) = {
           
            record match {

                case Some(name,marks,totalMarks,percentage,grade) =>
                        println(s"Name : $name") 
                        println(s"Marks : $marks")
                        println(s"Total Marks : $totalMarks")
                        println(s"Percentage : $percentage")
                        println(s"Grade : $grade")
                
                case None => println("Record not found")
            }
        }

        print("Enter the number of students : ")
        var count = readInt()

        println()

        var StudentRecords:List[(String,Int,Int,Double,Char)] = List()

        while(count > 0) {
            StudentRecords = getStudentInfo() :: StudentRecords
            println()
            count = count - 1
        }

        println("View Student Records")
        val searchRecord = readLine("Enter the name : ")
        println()

        val record = StudentRecords.find(_._1 == searchRecord)

        printStudentRecord(record)
    }
}