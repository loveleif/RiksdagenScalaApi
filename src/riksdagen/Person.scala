package riksdagen

import http.Http

trait Person {
  val id:  String
  def born: Int
  def sex: String
  def lastName: String
  def firstName: String
  def party: String
  def valkrets: String
  def urlXml: String  
}

case class PersonFull(
  id:  String,
  born: Int,
  sex: String,
  lastName: String,
  firstName: String,
  party: String,
  valkrets: String,
  urlXml: String
) extends Person

case class PersonLazy(id: String) extends Person {
  lazy val personFull = Person.find(id)
  
  def born = personFull.born
  def sex = personFull.sex
  def lastName = personFull.lastName
  def firstName  = personFull.firstName
  def party = personFull.party
  def valkrets = personFull.valkrets
  def urlXml = personFull.urlXml
}

object Person {
  def urlXml(id: String) = "http://data.riksdagen.se/person/" + id
  
  def find(terms: PersonSearchTerms) = {
    val xml = Http.getXml(terms.searchUrl)
    (xml \ "person").map(fromXml(_))
  }
  
  def find(personId: String) = {
    fromXml(Http.getXml(urlXml(personId)))
  }
  
  def findAll() = find(PersonSearchTerms())
  
  def fromXml(xml: scala.xml.Node) = {
    PersonFull(
      id = (xml \ "intressent_id").text,      
      born = (xml \ "fodd_ar").text.toInt,
      sex = (xml \ "kon").text,
      lastName = (xml \ "efternamn").text,
      firstName = (xml \ "tilltalsnamn").text,
      party = (xml \ "parti").text,
      valkrets = (xml \ "valkrets").text,
      urlXml = (xml \ "person_url_xml").text
    )
  }
}

case class PersonSearchTerms(
  iid: Option[String] = None,
  fnamn: Option[String] = None,
  enamn: Option[String] = None,
  f_ar: Option[Int] = None,
  kn: Option[String] = None,
  parti: Option[String] = None,
  valkrets: Option[String] = None,
  rdlstatus: String = "samtliga",
  org: Option[String] = None,
  utformat: Option[String] = None,
  termlista: Option[String] = None
) { 
  def searchUrl(): String = {
    "http://data.riksdagen.se/personlista/?" +
    "iid=" + iid.getOrElse("") +
    "&fnamn=" + fnamn.getOrElse("") +
    "&enamn=" +  enamn.getOrElse("") +
    "&f_ar=" + f_ar.getOrElse("") +
    "&kn=" + kn.getOrElse("") +
    "&parti=" + parti.getOrElse("") +
    "&valkrets=" + valkrets.getOrElse("") +
    "&rdlstatus=" + rdlstatus +
    "&org=" + org.getOrElse("") +
    "&utformat=" + utformat.getOrElse("") +
    "&termlista=" + termlista.getOrElse("")
  }
}
