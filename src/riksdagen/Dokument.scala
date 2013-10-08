package riksdagen

import java.net.URL
import http.Http
import scala.collection.immutable.VectorBuilder
import scala.io.BufferedSource

case class Dokument(
  id: String,
  rm: String,
  beteckning: String,
  typ: String,
  subTyp: String,
  organ: String,
  datum: String,
  titel: String,
  subTitel: String,
  status: String,
  intressenter: Seq[DokIntressent]
) {
  def urlHtml() = Dokument.urlHtml(this.id)
  def urlText() = Dokument.urlText(this.id)
  def urlXml() = Dokument.urlXml(this.id)

  def text = Http.get(urlText, _.mkString)
  //def intressenter = xml \ "dokintressent"
}

object Dokument {
  def urlHtml(dokId: String) = "http://data.riksdagen.se/dokument/" + dokId
  def urlText(dokId: String) = urlHtml(dokId) + "/text"
  def urlXml(dokId: String) = "http://data.riksdagen.se/dokumentstatus/" + dokId

  def fromXml(xml: scala.xml.Node): Dokument = {
    new Dokument(
      id = (xml \ "dokument" \ "dok_id").text,
      rm = (xml \ "dokument" \ "rm").text,
      beteckning = (xml \ "dokument" \ "beteckning").text,
      typ = (xml \ "dokument" \ "typ").text,
      subTyp = (xml \ "dokument" \ "subtyp").text,
      organ = (xml \ "dokument" \ "organ").text,
      datum = (xml \ "dokument" \ "datum").text,
      titel = (xml \ "dokument" \ "titel").text,
      subTitel = (xml \ "dokument" \ "subtitel").text,
      status = (xml \ "dokument" \ "status").text,
      intressenter = (xml \ "dokintressent").map(DokIntressent.fromXml(_))
    )
  }
  
  def fromXml(url: String): Dokument = fromXml(Http.getXml(url))
  
  def find(terms: DokSearchTerms): Seq[Dokument] = {
    SearchResult(terms.searchUrl).flatten.toSeq
  }
  
  def find(dokId: String): Dokument = fromXml(Dokument.urlXml(dokId))
  
  def findAllIds(terms: DokSearchTerms): Seq[String] = {
    Http.get(terms.copy(utformat = "iddump").searchUrl, readIdDump)
  }
  
  def readIdDump(source: BufferedSource): Seq[String] = {
    val stringBuilder: StringBuilder = new StringBuilder(30)
    val vectorBuilder: VectorBuilder[String] = new VectorBuilder()
    
    source.foreach { char =>
      if (char != ',')
        stringBuilder += char
      else {
        vectorBuilder += stringBuilder.mkString
        stringBuilder.clear
      }
    }
    vectorBuilder.result
  }
}

case class DokSearchTerms(
  rm: Option[String] = None,
  typ: Option[String] = None,
  d: Option[String] = None,
  ts: Option[String] = None,
  sn: Option[String] = None,
  parti: Option[String] = None,
  iid: Option[String] = None,
  bet: Option[String] = None,
  org: Option[String] = None,
  kat: Option[String] = None,
  sz: Int = 200,
  sort: String = "c",
  utformat: String = "xml",
  termlista: Option[String] = None
) {
  def searchUrl(): String = {
    "http://data.riksdagen.se/dokumentlista/?" +
    "rm=" + rm.getOrElse("") +
    "&typ=" + typ.getOrElse("") + 
    "&d=" + d.getOrElse("") +
    "&ts=" + ts.getOrElse("") + 
    "&sn=" + sn.getOrElse("") +
    "&parti=" + parti.getOrElse("") +
    "&iid=" + iid.getOrElse("") +
    "&bet=" + bet.getOrElse("") +
    "&org=" + org.getOrElse("") +
    "&kat=" + kat.getOrElse("") +
    "&sz=" + sz +
    "&sort=" + sort +
    "&utformat=" + utformat + 
    "&termlista=" + termlista.getOrElse("")
  }
}

class SearchResult(firstPage: String) extends Iterator[Seq[Dokument]] {
  private var nextPage = Option(firstPage)
  
  override def hasNext(): Boolean = {
    !nextPage.isEmpty
  }
  
  override def next(): Seq[Dokument] = {
    println(nextPage.get)
    val xml = Http.getXml(nextPage.get)

    val nastaSida = (xml \ "@nasta_sida")
    nextPage = if (nastaSida.isEmpty) None else Option(nastaSida.text)

    (xml \ "dokument").map(dok => Dokument.find((dok \ "dok_id").text))
  }
}

object SearchResult {
  def apply(firstPage: String) = new SearchResult(firstPage)
}
