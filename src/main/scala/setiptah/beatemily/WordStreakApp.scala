package setiptah.beatemily

/**
  * Created by nightfender on 2/2/16.
  */
object WordStreakApp extends App {

  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  //println(alphabet)

  def charGen = {
    val rng = new scala.util.Random

    def nextLetter = {
      val index = rng.nextInt(alphabet.length)
      alphabet.charAt(index)
      }

     Stream.from(0).map { k => nextLetter }
  }.iterator

  def getRandomBoard = {
    val board = Array.ofDim[Char](4,4)

    val chars = charGen

    for (i <- 0 until 4; j <- 0 until 4) {
      board(i)(j) = chars.next
    }

    board
  }

  def getDemoBoard = {

    val board = Array.ofDim[Char](3,3)

    val letters = "ABCDEFGHI"

    val zipped = letters.zipWithIndex

    zipped.map { t => (t._1, t._2 / 3, t._2 % 3) }.foreach { t => board(t._2)(t._3) = t._1 }

    board
  }

  def getLetterBoard(letters: String, nrow: Int) = {
    val n = letters.length

    def roundUp(num: Int, div: Int) = {
      (num + div - 1) / div
    }

    val ncol = roundUp(n,nrow)

    val board = Array.ofDim[Char](nrow,ncol)

    val zipped = letters.zipWithIndex

    zipped.map { t => (t._1, t._2 / ncol, t._2 % ncol) }.foreach { t => board(t._2)(t._3) = t._1 }

    board
  }

  def getHelloWords = List("HELLO","WORLD")

  def getDemoWords = List("ABC",  "AEI", "FBDHI", "ACFE", "AEFH", "AGGI", "ABA")

  def getFileWords(filename: String) = {
    import scala.io.Source

    Source.fromFile(filename).getLines().map { str => str.toUpperCase() }.toList
  }

  //val demoBoard = getRandomBoard
  val demoBoard = getLetterBoard("SSFLESONWTNDHAIE", 4)

  //val demoWords = getDemoWords
  val demoWords = getFileWords("wordsEn.txt")

  println( demoBoard.toList.map { vec => vec.toList } )
  //println ( Board(3,3).neighbors(1,0) )

  val dictionary = DictionaryTree.fromWords(demoWords)

  val found = WordStreakAlgorithm.findAllWords(demoBoard)(dictionary)

  for ( t <- found.keys ) {

    println("")
    println(t)
    found(t).filter( w => w.length >= 4 ).foreach(println)
  }
}
