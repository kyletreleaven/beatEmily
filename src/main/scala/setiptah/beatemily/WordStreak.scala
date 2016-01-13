package setiptah.beatemily

import scala.collection.mutable

class WordNode {
  //val moves : mutable.Map
}

object WordStreak extends App {
  
  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  
  val charGen = {
    val rng = new scala.util.Random
    
    def nextLetter = {
      val index = rng.nextInt(alphabet.length)
      alphabet.charAt(index)
      }
    
     Stream.from(0).map { k => nextLetter }
  }.iterator
  
  val demoBoard = Array.ofDim[Char](4,4)
  
  for (i <- 0 until 4; j <- 0 until 4) {
    demoBoard(i)(j) = charGen.next
  }
  
  println(alphabet)
  
  println( demoBoard.toList.map { vec => vec.toList } )
}