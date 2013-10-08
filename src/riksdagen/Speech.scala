package riksdagen

import http.Http

case class Speech(
  id: String,
  title: String,
  subTitle: String,
  date: String,
  speakerId: String,
  urlXml: String
) {
  def requestText(): String = {
    (Http.getXml(urlXml) \ "anforandetext").text
  }
}

object Speech {
  def find(terms: SpeechSearchTerms): Seq[Speech] = {
    val xml = Http.getXml(terms.searchUrl)
    
    val hits = (xml \ "@antal").text.toInt
    if (hits > terms.sz) throw new Exception("Too many seach hits")
    
    (xml \ "anforande").map(fromXml(_))
  }
  
  def findByPerson(personId: String) = find(SpeechSearchTerms(iid = Some(personId)))
  
  def fromXml(xml: scala.xml.Node): Speech = {
    Speech(
      id = (xml \ "dok_id").text,
      title = (xml \ "dok_titel").text,
      subTitle = (xml \ "avsnittsrubrik").text,
      date = (xml \ "dok_datum").text,
      speakerId = (xml \ "intressent_id").text,
      urlXml = (xml \ "anforande_url_xml").text
    )
  }
}

case class SpeechSearchTerms(
  rm: Option[String] = None,
  anftyp: Option[String] = None,
  d: Option[String] = None,
  ts: Option[String] = None,
  parti: Option[String] = None,
  iid: Option[String] = None,
  sz: Int = 10000,
  utformat: String = "xml"
) {
  def searchUrl(): String = {
    "http://data.riksdagen.se/anforandelista/?" +
    "rm=" + rm.getOrElse("") +
    "&anftyp=" + anftyp.getOrElse("") + 
    "&d=" + d.getOrElse("") +
    "&ts=" + ts.getOrElse("") + 
    "&parti=" + parti.getOrElse("") +
    "&iid=" + iid.getOrElse("") +
    "&sz=" + sz +
    "&utformat=" + utformat
  }
}