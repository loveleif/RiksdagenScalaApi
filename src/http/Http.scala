package http

import scala.io.BufferedSource
import java.net.URL

/**
 * The `Http` object provides various functions for making http requests.
 * 
 * @author Kristofer Leifland
 */
object Http {
  
  /**
   * Performs a http get request and uses the response body on the provided handler
   * 
   * @param url url
   * @param handler function to handle the respone body
   *
  def get(url: String, handler: BufferedSource => Unit): Unit = {
    val conn = new URL(url).openConnection();
    val stream = conn.getInputStream();
    handler(io.Source.fromInputStream(stream))
    stream.close()
  }
  */
  def get[T](url: String, handler: BufferedSource => T, enc: String = "UTF-8"): T = {
    val conn = new URL(url).openConnection();
    val stream = conn.getInputStream();
    val out = handler(io.Source.fromInputStream(stream)(enc))
    stream.close()
    out
  }
  
  /**
   * Performs a http get request. Parses and returns the respons as XML.
   * 
   * @param url function to handle the respone body
   * @return parsed xml response
   */  
  def getXml(url: String): scala.xml.Elem = {
    val conn = new URL(url).openConnection()
    val stream = conn.getInputStream()
    val xml = scala.xml.XML.load(stream)
    stream.close()
    xml
  }
}