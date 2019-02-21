package routes

import java.io.PrintWriter

import model._
import org.json4s.DefaultFormats

/**
  * Created by Daniel on 24/11/2018.
  */
class Routes(crsCodesPath: String, graphPath: String, jsonPath: String, tiploc: String, fixedLinkData: String, processedStationsFile: String, additionalFixedLinkData: String) {


  val stationCodes = Station.readStationCodes(crsCodesPath)
  val graphCodes = GraphConnection.readGraphStations(graphPath)
  val richStations = Location.readStationsFromJson(jsonPath)
  val graph = GraphConnection.readGraph(graphPath)
  val tiplocs: Map[String, RichTiploc] = TiplocLocation.readTiplocFromJson(tiploc)
  val fixedLinks: Set[GraphConnection] = GraphConnection.readFixedLinks(fixedLinkData)
  val additionalFixedLinks: Set[GraphConnection] = GraphConnection.readAdditionalFixedLinks(additionalFixedLinkData)
  val processedStations: Map[String, EnrichedLocation] = Location.readProcessedStations(processedStationsFile)

  def enrichGraph: Set[RichGraphConnection] = {
    println("Dodgy graph connections")
    graph ++ fixedLinks ++ additionalFixedLinks map {
      connection =>
        val from: RichStationWithType = richStationById(connection.from)
        val to: RichStationWithType = richStationById(connection.to)
//        println(connection)
        if (from.name.equals("Unknown") || to.name.equals("Unknown")) {
          println(connection)
        }

        val srs = if(connection.srsCode.equals("")) None else Some(connection.srsCode)
        val connType = getConnectionType(from, to, connection)

        RichGraphConnection(
          from,
          to,
          connection.toc,
          connection.singleTrack,
          connection.electrification,
          connection.speed,
          srs,
          connType)
    } filterNot(p => p.from.equals(p.to)) filterNot(p => p.from.name.equals("Unknown") || p.to.name.equals("Unknown"))
  }

  def getConnectionType(from: RichStationWithType, to: RichStationWithType, connection: GraphConnection): String = {
    def fromToTypeMatches(value: String): Boolean = from.`type`.equals(value) && to.`type`.equals(value)
    if(fromToTypeMatches("Underground") || connection.toc.startsWith("LU-")) "Underground"
    else if(fromToTypeMatches("Tram")) "Tram"
    else if(fromToTypeMatches("Metro")) "Metro"
    else if(fromToTypeMatches("Light Rail")) "Light Rail"
    else if(fromToTypeMatches("Heritage")) "Heritage"
    else if(connection.srsCode.contains(".")) "NR"
    else "Unknown"

  }

  def richStationById(id: String): RichStationWithType = {
    val station: Option[EnrichedLocation]= processedStations.values.find(
      p => {
        p.id.equals(id) ||
        p.crs.contains(id)
      }
    )
    station map {
      el => RichStationWithType(el.location.lat, el.location.lon, el.id, el.name, el.`type`)
    } getOrElse RichStationWithType(0.0, 0.0, id, "Unknown", "")
  }
}

object Routes {
  def tiploc = "../raw-data/enrichedTiplocs.json"

  def graphFilePath = "../raw-data/RAIL ROUTE GRAPH - Sheet1(1).csv"

  def stationJsonPath = "../raw-data/stations(4).json"

  def stationCodes = "../raw-data/station_codes.csv"

  def output = "../data/static/routes.json"

  def additionalLinks = "../raw-data/ttis144//ttisf144.flf"

  def aditionalFixedLinks = "../raw-data/ttis144/ttisf144.alf"

  def processedStations = "../data/static/locations.json"
}

object Main extends App {
  val stations = new Routes(
    Routes.stationCodes,
    Routes.graphFilePath,
    Routes.stationJsonPath,
    Routes.tiploc,
    Routes.additionalLinks,
    Routes.processedStations,
    Routes.aditionalFixedLinks
  )
  val count = stations.graphCodes.size
  val codes = stations.stationCodes.size
  println(s"Station count: $count")
  println(s"Station Codes: $codes")
  stations.stationCodes foreach {
    station =>
      if (!stations.graphCodes.contains(station))
        println(s"${station.crs}")
  }

  val richStations = stations.enrichGraph


  import org.json4s.native.Serialization.writePretty

  implicit val formats = DefaultFormats

  val jsonString: String = writePretty(richStations)
  new PrintWriter(Routes.output) {
    write(jsonString); close
  }


}

