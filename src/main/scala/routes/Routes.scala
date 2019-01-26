package routes

import java.io.PrintWriter

import model._
import org.json4s.DefaultFormats

/**
  * Created by Daniel on 24/11/2018.
  */
class Routes(crsCodesPath: String, graphPath: String, jsonPath: String, tiploc: String, additionalData: String, processedStationsFile: String) {


  val stationCodes = Station.readStationCodes(crsCodesPath)
  val graphCodes = GraphConnection.readGraphStations(graphPath)
  val richStations = Location.readStationsFromJson(jsonPath)
  val graph = GraphConnection.readGraph(graphPath)
  val tiplocs: Map[String, RichTiploc] = TiplocLocation.readTiplocFromJson(tiploc)
  val additionalLinks: Set[GraphConnection] = GraphConnection.readAdditionalData(additionalData)
  val processedStations: Map[String, EnrichedLocation] = Location.readProcessedStations(processedStationsFile)

  def enrichGraph: Set[RichGraphConnection] = {
    println("Dodgy graph connections")
    graph ++ additionalLinks map {
      connection =>
        val from: RichStationWithType = richStationByCrs(connection.from)
        val to: RichStationWithType = richStationByCrs(connection.to)
//        println(connection)

        if (from.name.equals("Unknown") || to.name.equals("Unknown")) {
          println(connection)
        }

        RichGraphConnection(
          from,
          to,
          connection.toc,
          connection.singleTrack,
          connection.electrification,
          connection.speed,
          connection.srsCode)
    } filterNot(p => p.from.equals(p.to)) filterNot(p => p.from.name.equals("Unknown") || p.to.name.equals("Unknown"))
  }

  def richStationByCrs(crs: String): RichStationWithType = {
    val station: Option[EnrichedLocation]= processedStations.values.find(
      p => {
        p.crs.equals(crs) ||
        p.subsidiaryCrs.contains(crs)
      }
    )
    station map {
      el => RichStationWithType(el.location.lat, el.location.lon, el.tiploc, el.name, el.crs, el.toc, el.`type`)
    } getOrElse RichStationWithType(0.0, 0.0, "Unknown", "Unknown", crs, "", "")
  }
}

object Routes {
  def tiploc = "../railvisits/raw-data/TiplocOutput.json"

  def graphFilePath = "../railvisits/raw-data/RAIL ROUTE GRAPH - Sheet1(1).csv"

  def stationJsonPath = "../railvisits/raw-data/stations(4).json"

  def stationCodes = "../railvisits/raw-data/station_codes.csv"

  def output = "../railvisits/data/routes.json"

  def additionalLinks = "../railvisits/raw-data/ttis144//ttisf144.flf"

  def processedStations = "../railvisits/data/locations.json"
}

object Main extends App {
  val stations = new Routes(Routes.stationCodes, Routes.graphFilePath, Routes.stationJsonPath, Routes.tiploc, Routes.additionalLinks, Routes.processedStations)
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

