package setiptah.beatemily

import scala.collection.mutable

object DictionaryTree {
  
  def fromWords(wordList: Iterable[String]) : DictionaryTree = {
    val dt = new DictionaryTree
    
    wordList.foreach { word => dt.top.extend(word) }
    
    return dt
  }
  
}

class DictionaryTree {
  
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
  
  val top = new DictionaryTreeNode

  override def toString() : String = {
    return top.toString()      
  }

}

object DTSelfTest {
  
  val regex_whitespace = "\\s+" 
  
  def main(args: Array[String]) : Unit = {
    val words = "the boy bounced the basketball".toUpperCase().split(regex_whitespace)
    
    val dt = DictionaryTree.fromWords(words)
    
    println(dt)
    
  }
  
}