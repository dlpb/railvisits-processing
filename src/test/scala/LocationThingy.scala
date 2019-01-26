import model.EnrichedLocation
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class LocationThingy extends FlatSpec with Matchers {
  it should "work" in {
    val locations = readLocations("C:\\Users\\danie\\Documents\\code\\railvisits\\data\\locations.json")

    locations.filter(_.station).map(_.name).sorted.foreach { println }

  }

  def readLocations(path: String): List[EnrichedLocation] = {
    var json: String = ""
    implicit val formats = DefaultFormats
    var data: List[EnrichedLocation] = List.empty

    try{
      json = Source.fromFile(path).mkString
      data = parse(json).extract[List[EnrichedLocation]]
    }
    catch {
      case e: Exception => {
        e.printStackTrace()
      }
    }
    data
  }
}
