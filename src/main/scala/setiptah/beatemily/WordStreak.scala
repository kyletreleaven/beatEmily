package setiptah.beatemily

import scala.collection
import scala.collection.mutable
//import scala.collection.immutable


object WordStreakAlgorithm {

  def replaceBool(list2: List[List[Boolean]], i: Int, j: Int, value: Boolean) : List[List[Boolean]] = {

    // do the inefficient thing first

    val array2 = list2.toArray.map{ l => l.toArray }

      //.map { sublist => sublist.toArray }.toArray

    array2(i)(j) = value

    return array2.toList.map { vec => vec.toList }
  }

  case class SearchState(
                          val wordSoFar: String,
                          val cellsUsed: List[List[Boolean]],
                          val nextCell: (Int,Int),
                          val start: (Int,Int))

  /*
  def getActions(cursor: SearchState, board: Array[Array[Char]], dictionary: DictionaryTree) = {

    val nrow = board.length
    val ncol = board(0).length    
    val grid = Board(nrow,ncol)
    
    (grid.neighbors(cursor.nextCell._1, cursor.nextCell._2)
        
        // if that direction isn't already used
        .filter { case (i,j) => !cursor.cellsUsed(i)(j) }
        
        // and the next continuation of the path is in the dictionary
        .filter { action => dictionary.isPath( applyAction(action, cursor, board ).wordSoFar ) })
  }
  
  def applyAction(action: (Int,Int), cursor: SearchState, board: Array[Array[Char]]) : SearchState = {
    val nextWord = cursor.wordSoFar + board(action._1)(action._2)

    val nextUsed = replaceBool(cursor.cellsUsed, action._1, action._2, true )

    val nextCell = ( cursor.nextCell._1 + action._1, cursor.nextCell._2 + action._2 )
    
    return SearchState(nextWord,nextUsed,nextCell)
  }
  */

  def findAllWords(board: Array[Array[Char]])(dictionary: DictionaryTree) : collection.Map[(Int, Int), Array[String]] = {

    val searchStack = new mutable.Stack[SearchState]
    
    val nrow = board.length
    val ncol = board(0).length

    val grid = Board(nrow,ncol)

    val words = mutable.Map.empty[(Int,Int),mutable.ArrayBuffer[String]]
    val wordSet = mutable.HashSet.empty[String]

    // populate search queue with all possible starts
    val freshStart = Array.fill(nrow,ncol)(false).toList.map { vec => vec.toList }

    for ( i <- 0 until nrow ) {
      for ( j <- 0 until ncol ) {
        val addr = (i,j)

        words(addr) = mutable.ArrayBuffer.empty[String]

        val cursor = SearchState("",freshStart,addr,addr)

        searchStack.push(cursor)
      }
    }

    while (searchStack.length > 0 ) {

      val cursor = searchStack.pop()

      /*
      println("")
      println("next cursor")
      println( cursor.wordSoFar )
      println( cursor.cellsUsed )
      println( cursor.nextCell )
      */

      val address = cursor.nextCell

      val word = cursor.wordSoFar + board(address._1)(address._2)

      if (dictionary.isWord(word)) {
        if ( !wordSet.contains(word) ) {
          wordSet += word
          words(cursor.start) += word
        }
      }

      if (dictionary.isPath(word)) {

        val cellsUsed = replaceBool(cursor.cellsUsed, address._1, address._2, true)

        val validMoves = grid.neighbors(address._1,address._2).filter { case (i,j) => !cellsUsed(i)(j) }

        for ( nextCell <- validMoves ) {

          val newSearch = SearchState(word,cellsUsed,nextCell,cursor.start)

          searchStack.push(newSearch)
        }

      }
    }

    return words.mapValues { value => value.toArray }
  }
}
