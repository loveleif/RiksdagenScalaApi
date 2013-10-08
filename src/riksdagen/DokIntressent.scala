package riksdagen

case class DokIntressent(
    person: PersonLazy,
    roll: String
    ) {
}

object DokIntressent {

  def fromXml(xml: scala.xml.Node) = {
    DokIntressent(
        PersonLazy((xml \ "intressent_id").text),
        (xml \ "roll").text
    )
    
  }
}