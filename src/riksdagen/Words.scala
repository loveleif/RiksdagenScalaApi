package riksdagen

import scala.collection.mutable.{HashMap, Map}

class Words {
  private val words: Map[String, Int] = HashMap()
  
  def addLine(line: String): Unit = {
    line.split("\\s+").foreach(word => addWord(word))
  }
  
  def addWord(word: String, addCount: Int = 1): Unit = {
    words.get(word) match {
      case Some(count) => words += (word -> (count+addCount))
      case None => words += (word -> addCount)
    }
  }
  
  def addWords(moreWords: Words) = {
    words ++= moreWords.words
  }
  
  def toList = words.toList
  
  override def toString(): String = {
    words.toString
  }
}

object Words {
 def apply() = new Words()
}