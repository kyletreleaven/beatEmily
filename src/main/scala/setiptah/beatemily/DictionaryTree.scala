package setiptah.beatemily

import scala.collection.mutable

object DictionaryTree {
  
  def fromWords(wordList: Iterable[String]) : DictionaryTree = {
    val dt = new DictionaryTree
    
    wordList.foreach { word => dt.top.extend(word) }
    
    return dt
  }

  class DictionaryTreeNode {

    var wordNode = false

    val children = mutable.Map.empty[Char,DictionaryTreeNode]

    override def toString() : String = {

      def getChildString(kvp: (Char,DictionaryTreeNode)) : String = {
        return "%c => %s".format( kvp._1, kvp._2 )
      }

      val childStrings = children.map(getChildString)
      val childrenString = "[%s]".format( childStrings.mkString(", ") )

      return "{%b, %s}".format(wordNode, childrenString)
    }

    def extend(suffix: String) : Unit = {

      if ( suffix.isEmpty() ) {
        wordNode = true
      }

      else {
        val c = suffix.head
        val nextSuffix = suffix.tail

        val nextNode = children.getOrElseUpdate(c, new DictionaryTreeNode )

        nextNode.extend(nextSuffix)
      }

    }
  }

}

class DictionaryTree {

  import DictionaryTree._

  val top = new DictionaryTreeNode

  override def toString() : String = {
    return top.toString()      
  }
  
  def isWord(suffix: String, node: DictionaryTreeNode) : Boolean = {
    if (suffix.length() == 0) {
      return node.wordNode
    }
    
    val head = suffix.head
    
    if ( !node.children.contains(head) ) {
      return false
    }
    
    return isWord(suffix.tail, node.children.getOrElse(head, null) )
  }
  
  def isWord(word: String) : Boolean = isWord(word,top)
  
  def isPath(path: String, node: DictionaryTreeNode) : Boolean = {
    if (path.length() == 0) {
      return true
    }
    
    val head = path.head
    
    if (!node.children.contains(head)) {
      return false
    }
    
    return isPath(path.tail, node.children.getOrElse(head,null) )
  }

  def isPath(path: String) : Boolean = isPath(path, top)
  
  def isSuffix(suffix: String, node: DictionaryTreeNode) : Boolean
    = isPath(suffix,node) && !isWord(suffix,node)
    
  def isSuffix(suffix: String) : Boolean = isSuffix(suffix, top)
}

object DTSelfTest {
  
  val regex_whitespace = "\\s+" 
  
  def main(args: Array[String]) : Unit = {
    val words = "the boy bounced the basketball".toUpperCase().split(regex_whitespace)
    
    val dt = DictionaryTree.fromWords(words)
    
    println(dt)
    
    println(dt.isWord("BOY"))
  }
  
}