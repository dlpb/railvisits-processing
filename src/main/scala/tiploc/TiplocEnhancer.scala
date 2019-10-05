package tiploc

import java.io.PrintWriter

import model.{Location, RichTiploc, TiplocLocation}
import org.json4s.DefaultFormats

object TiplocEnhancer {
  def tiploc = "C:\\Users\\danie\\Documents\\code\\raw-data\\TIPLOC Eastings and Northings - TIPLOC(1).csv"
  def output = "C:\\Users\\danie\\Documents\\code\\raw-data\\enrichedTiplocs.json"
  def stationJsonPath = "C:\\Users\\danie\\Documents\\code\\raw-data\\station_input.json"
}

class TiplocEnhancer(file: String, jsonFile: String, output: String){

  val tiplocs = TiplocLocation.readTiplocFromCsv(file)
  val stations = Location.readStationsFromSourceJson(jsonFile)


  val enhancedTiplocs: Set[RichTiploc] = {

    val richTiplocs = tiplocs.keySet map {
      t =>
        val tiploc = tiplocs(t)
        val station = stations.get(t)

        val crs = station match {
          case Some(x) => x.crs
          case _ => t
        }

        val toc = station match {
          case Some(x) => x.toc
          case _ => "XX"
        }

        val locType = station match {
          case Some(x) => x.`type`
          case _ =>
            val name = tiploc.name.toUpperCase()
            if (name.endsWith("JN") || name.endsWith("JN.") || name.contains("JUNCTION"))
              Some("Junction")
            else if (name.contains("S.B.") || name.endsWith("SB"))
              Some("Signal Box")
            else if (name.contains("L.C.") || name.endsWith("LC") || name.contains("CROSSING"))
              Some("Crossing")
            else if (name.contains("SIGNAL") || name.contains("SIG"))
              Some("Signal")
            else
              Some("Infrastructure")
        }

        val name = station match {
          case Some(x) => x.name
          case _ => tiploc.name
        }

        val loc = TiplocLocation(tiploc.lat, tiploc.lon, tiploc.easting, tiploc.northing, toc, locType)

        val rt = RichTiploc(t, crs, name.toLowerCase().split(" ").map {
          _.capitalize
        }.mkString(" "), loc)
        println(rt)
        rt
    }


    richTiplocs
  }

  import org.json4s.native.Serialization.writePretty

  implicit val formats = DefaultFormats

  val jsonString: String = writePretty(enhancedTiplocs).replaceAllLiterally("\\\"", "")
  new PrintWriter(output) {
    write(jsonString)
    close
  }
}

object Main extends App {
  new TiplocEnhancer(TiplocEnhancer.tiploc, TiplocEnhancer.stationJsonPath, TiplocEnhancer.output)
}

