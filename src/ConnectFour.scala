import scala.util.control.Breaks._

class Game{
  var p: Array[String] = Array.fill[String](42)("  ") //Array.range(1, 43).map(n => if(n < 10) ' ' + n.toString else n.toString) // positions
  var board =
    s"""
       |   1    2    3    4    5    6    7
       |
       ||    |    |    |    |    |    |    |
       || ${p(0)} | ${p(1)} | ${p(2)} | ${p(3)} | ${p(4)} | ${p(5)} | ${p(6)} |
      ||    |    |    |    |    |    |    |
       |------------------------------------
       ||    |    |    |    |    |    |    |
       || ${p(7)} | ${p(8)} | ${p(9)} | ${p(10)} | ${p(11)} | ${p(12)} | ${p(13)} |
      ||    |    |    |    |    |    |    |
       |------------------------------------
       ||    |    |    |    |    |    |    |
       || ${p(14)} | ${p(15)} | ${p(16)} | ${p(17)} | ${p(18)} | ${p(19)} | ${p(20)} |
      ||    |    |    |    |    |    |    |
       |------------------------------------
       ||    |    |    |    |    |    |    |
       || ${p(21)} | ${p(22)} | ${p(23)} | ${p(24)} | ${p(25)} | ${p(26)} | ${p(27)} |
      ||    |    |    |    |    |    |    |
       |------------------------------------
       ||    |    |    |    |    |    |    |
       || ${p(28)} | ${p(29)} | ${p(30)} | ${p(31)} | ${p(32)} | ${p(33)} | ${p(34)} |
      ||    |    |    |    |    |    |    |
       |------------------------------------
       ||    |    |    |    |    |    |    |
       || ${p(35)} | ${p(36)} | ${p(37)} | ${p(38)} | ${p(39)} | ${p(40)} | ${p(41)} |
      ||    |    |    |    |    |    |    |
       |------------------------------------
    """.stripMargin
  var isFinished = false

  def displayBoard: Unit ={
    println(board)
  }

  def updateBoard: Unit ={
    board =
      s"""
         |   1    2    3    4    5    6    7
         |
         ||    |    |    |    |    |    |    |
         || ${p(0)} | ${p(1)} | ${p(2)} | ${p(3)} | ${p(4)} | ${p(5)} | ${p(6)} |
      ||    |    |    |    |    |    |    |
         |------------------------------------
         ||    |    |    |    |    |    |    |
         || ${p(7)} | ${p(8)} | ${p(9)} | ${p(10)} | ${p(11)} | ${p(12)} | ${p(13)} |
      ||    |    |    |    |    |    |    |
         |------------------------------------
         ||    |    |    |    |    |    |    |
         || ${p(14)} | ${p(15)} | ${p(16)} | ${p(17)} | ${p(18)} | ${p(19)} | ${p(20)} |
      ||    |    |    |    |    |    |    |
         |------------------------------------
         ||    |    |    |    |    |    |    |
         || ${p(21)} | ${p(22)} | ${p(23)} | ${p(24)} | ${p(25)} | ${p(26)} | ${p(27)} |
      ||    |    |    |    |    |    |    |
         |------------------------------------
         ||    |    |    |    |    |    |    |
         || ${p(28)} | ${p(29)} | ${p(30)} | ${p(31)} | ${p(32)} | ${p(33)} | ${p(34)} |
      ||    |    |    |    |    |    |    |
         |------------------------------------
         ||    |    |    |    |    |    |    |
         || ${p(35)} | ${p(36)} | ${p(37)} | ${p(38)} | ${p(39)} | ${p(40)} | ${p(41)} |
      ||    |    |    |    |    |    |    |
         |------------------------------------
    """.stripMargin
  }

  def isPositionEmpty(position: Int): Boolean = {
    if (p(position - 1) == "RR" || p(position - 1) == "YY") {
      false
    } else {
      true
    }
  }

  def findLowestPosition(position: Int): Int = {
    var newPosition = -1
    if (isPositionEmpty(position)) {
      newPosition = position
      breakable {
        while (newPosition + 7 < 43) {
          if (isPositionEmpty(newPosition + 7)) {
            newPosition += 7
          } else {
            break
          }
        }
      }
    }
    newPosition
  }

  def playMove(colour: Char, position: Int): Unit ={
    if (position < 1 || position > 7 ) {
      displayBoard
      println
      println("Invalid move.")
      println
    } else {
      val lowestPosition = findLowestPosition(position)
      if (lowestPosition == -1) {
        displayBoard
        println
        println("Invalid move.")
        println
      } else {
        p(lowestPosition - 1) = colour.toString + colour.toString
        updateBoard
        displayBoard
        checkForFour
      }
    }
  }

  def checkForFour: Unit = {
    // vertical checks
    val verticals = Array.range(0, 21)
    verticals.foreach(i => if (!isPositionEmpty(i +1) && p(i) == p(i +7) && p(i) == p(i +14) && p(i) == p(i +21)) endGame(p(i)))
    // horizontal checks
    val horizontals = Array(0, 1, 2, 3, 7, 8, 9, 10, 14, 15, 16, 17, 21, 22, 23, 24, 28, 29, 30, 31, 35, 36, 37,38)
    horizontals.foreach(i => if (!isPositionEmpty(i +1) && p(i) == p(i +1) && p(i) == p(i +2) && p(i) == p(i +3)) endGame(p(i)))
    // diagonal checks
    // top left to bottom right checks (major diagonals)
    val majorDiagonals = Array(0, 1, 2, 3, 7, 8, 9, 10, 14, 15, 16, 17)
    majorDiagonals.foreach(i => if (!isPositionEmpty(i +1) && p(i) == p(i + 8) && p(i) == p(i + 16) && p(i) == p(i + 24)) endGame(p(i)))
    // top right  to bottom left checks (minor diagonals)
    val minorDiagonals = Array(3, 4, 5, 6, 10, 11, 12, 13, 17, 18, 19, 20)
    minorDiagonals.foreach(i => if (!isPositionEmpty(i +1) && p(i) == p(i + 6) && p(i) == p(i + 12) && p(i) == p(i + 18)) endGame(p(i)))
    // draw check
    val allPositions = Array.range(1, 43)
    var isDraw = true
    allPositions.foreach(i => if (isPositionEmpty(i)) isDraw = false)
    if (isDraw) {
      endGame("draw")
    }
  }

  def endGame(winner: String): Unit ={
    if (winner == "draw") {
      println("Draw.")
    } else {
      println(s"${winner(0)} wins!")
    }
    isFinished = true
  }
}

object ConnectFour extends App {
  var currentGame = new Game
  var currentPlayer = 'R'
  var moveInput = 0

  currentGame.displayBoard

  while (!(currentGame.isFinished)) {
    moveInput = scala.io.StdIn.readLine(s"$currentPlayer enter your move: ").toInt
    currentGame.playMove(currentPlayer, moveInput)

    if (currentPlayer == 'R') {
      currentPlayer = 'Y'
    } else {
      currentPlayer = 'R'
    }
  }
}
