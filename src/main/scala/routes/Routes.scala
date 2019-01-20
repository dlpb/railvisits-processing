package routes

import java.io.PrintWriter

import model._
import org.json4s.DefaultFormats

/**
  * Created by Daniel on 24/11/2018.
  */
class Stations(crsCodesPath: String, graphPath: String, jsonPath: String, tiploc: String, additionalData: String, processedStationsFile: String) {


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
//    val station = richStations.values.find(s => crs.equals(s.crs))
//    val loc: RichStationWithType = if (station.isEmpty) {
//      val fromTiploc = tiplocs.find(s => crs.equals(s._2.crs)).map {
//        l =>
//          RichStationWithType(l._2.location.lat.toDouble, l._2.location.lon.toDouble, l._2.tiploc, l._2.name, l._2.crs, l._2.location.toc, l._2.location.`type`.getOrElse(""))
//      } //.getOrElse(RichStationWithType(0.0,0.0,"","Unknown","XXX","","Unknown"))
//      if (fromTiploc.isEmpty) {
//        val fromProcessedStation = processedStations.find(p => crs.equals(p.crs))
//        if(fromProcessedStation.isEmpty){
//          RichStationWithType(0.0,0.0,"","Unknown","XXX","","Unknown")
//        }
//        else {
//          val el = fromProcessedStation.get
//          RichStationWithType(el.location.lat, el.location.lon, el.tiploc, el.name, el.crs, el.toc, el.`type`)
//        }
//      }
//      else fromTiploc.get
//    } else {
//      station
//    }.getOrElse(RichStationWithType(0.0, 0.0, "", "Unknown", "XXX", "", "Unknown"))
//
//    loc
//    2


//    val station: RichStationWithType = processedStations.get(crs).map {
//      el =>
//        RichStationWithType(el.location.lat, el.location.lon, el.tiploc, el.name, el.crs, el.toc, el.`type`)
//    }.getOrElse {
//      val matches = processedStations.filter(p => p._2.subsidiaryCrs.contains(crs))
//      if(matches.isEmpty)
//        if(richStations.contains(crs)) richStations(crs)
//        else RichStationWithType(0.0, 0.0, "Unknown", "Unknown", crs, "", "")
//      else {
//        val el = matches.head._2
//        RichStationWithType(el.location.lat, el.location.lon, el.tiploc, el.name, el.crs, el.toc, el.`type`)
//      }
//    }
//    station

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

object Stations {
  def tiploc = "C:\\Users\\Daniel\\Downloads\\TiplocOutput.json"

  def graphFilePath = "C:\\Users\\Daniel\\Downloads\\RAIL ROUTE GRAPH - Sheet1(1).csv"

  def stationJsonPath = "C:\\Users\\Daniel\\Downloads\\stations(4).json"

  def stationCodes = "C:\\Users\\Daniel\\Downloads\\station_codes.csv"

  def output = "C:\\Users\\Daniel\\Downloads\\richData.json"

  def additionalLinks = "C:\\Users\\Daniel\\Downloads\\ttis144\\ttisf144.flf"

  def processedStations = "C:\\Users\\Daniel\\Downloads\\output.json"
}

object Main extends App {
  val stations = new Stations(Stations.stationCodes, Stations.graphFilePath, Stations.stationJsonPath, Stations.tiploc, Stations.additionalLinks, Stations.processedStations)
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
  new PrintWriter(Stations.output) {
    write(jsonString); close
  }


}

