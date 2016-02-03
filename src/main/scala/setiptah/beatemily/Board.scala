package setiptah.beatemily

object Board {

  // don't really understand how this syntax works...
  object LiterateMove extends Enumeration {
    type LiterateMove = Value
    val E, NE, N, NW, W, SW, S, SE = Value

    val allMoves = List(E,NE,N,NW,W,SW,S,SE)
  }

}

case class Board(numRows: Int, numCols: Int) {

  import Board._

  // does the numRows x numCols grid contain the address (i,j) ?
  def has(i: Int, j: Int) = {
    i >= 0 && i < numRows && j >= 0 && j < numCols
  }
  
  def possibleMoves = LiterateMove.allMoves.map {
    move => {

      import LiterateMove._
      
      move match {
        case E => (+1,0)
        case NE => (+1,+1)
        case N => (0,+1)
        case NW => (-1,+1)
        case W => (-1,0)
        case SW => (-1,-1)
        case S => (0,-1)
        case SE => (+1,-1)
      }
    }
  }
  
  def neighbors(i: Int, j: Int) = possibleMoves
    .map { case (dx,dy) => (i-dy,j+dx) }
    .filter { case (ii,jj) => has(ii,jj) }
}

object BoardTestApp {
  def main(args: Array[String]) : Unit = {
    val board = Board(4,4)
    
    board.neighbors(1,1).foreach { println(_) }
    //println( board.nei
  }
}
