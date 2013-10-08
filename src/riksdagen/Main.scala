package riksdagen

import http.Http


object Main {
  def main(args: Array[String]) {
    val docs = Dokument.find(DokSearchTerms(typ = Some("mot"), rm = Some("2012"), parti=Some("MP")))
    println(docs.size)
    println(docs(0).urlText)
    println(docs(0).text)
    val words = Words()
    
    docs.foreach(doc => words.addLine(doc.text))
    
    words.toList.sortBy(_._2) foreach {
      case (key, value) => println(key + " = " + value)
    }
  }
}