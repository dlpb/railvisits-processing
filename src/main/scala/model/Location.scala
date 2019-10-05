package model

import java.io.FileNotFoundException

import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse

import scala.collection.mutable
import scala.io.Source

case class RichStation(lat: Double, lon: Double, tiploc: String, name: String, crs: String, toc: String, `type`: Option[String])

case class RichStationWithType(lat: Double, lon: Double, id: String, name: String, `type`: String)

case class RichStations(locations: List[RichStation])


case class Locations(locations: List[Location])

case class Location(lat: String, lon: String, tiploc: String, name: String, crs: String, toc: String, `type`: Option[String])

case class TTISFLocation(name: String, categoryType: String, tiploc: String, subsidiaryCrs: String, crs: String, easting: String, estimated: Boolean, northing: String, changeTime: String, lat: Option[String] = None, lon: Option[String] = None) {
  override def toString(): String = {
    s"""A    ${name}${toCATEType(categoryType)}$tiploc$subsidiaryCrs   $crs$easting${toEstimated(estimated)}$northing$changeTime,${lat.getOrElse("")}, ${lon.getOrElse("")}""".stripMargin
  }

  def toInterchangePoint(s: String) = {
    s match {
      case "0" => "0 - Not an interchange point"
      case "1" => "1 - Small interchange point"
      case "2" => "2 - Medium interchange point"
      case "3" => "3 - Large interchange point"
      case "9" => "9 - Subsidiary TIPLOC for station"
    }
  }

  def toCATEType(s: String) = {
    s.substring(0, 1)
  }

  def toEstimated(b: Boolean) = if (b) "E" else " "
}

case class NrInfo(crp: String, route: String, srs: String, changeTime: String, interchangeType: String)

case class SpatialLocation(lat: Double, lon: Double, county: Option[String], district: Option[String], postcode: Option[String])

case class EnrichedLocation(
                             id: String,
                             name: String,
                             operator: String,
                             `type`: String,
                             location: SpatialLocation,
                             nrInfo: Option[NrInfo],
                             orrStation: Boolean,
                             crs: Set[String] = Set(),
                             tiploc: Set[String] = Set()) {
  override def hashCode(): Int = super.hashCode()

  override def equals(obj: Any): Boolean = this.canEqual(obj) && this.id.equals(obj.asInstanceOf[EnrichedLocation].id)

  override def canEqual(that: Any): Boolean = that.isInstanceOf[EnrichedLocation]
}

case class Estimate17StationEntry(tlc: String, name: String, region: String, authority: String, constituency: String, osEasting: Double, osNorthing: Double, toc: String, srsCode: String, srsDescription: String, nrRoute: String, crpLine: String, entriesAndExits: Long)

case class Estimate11StationEntry(tlc: String, name: String, location: String, region: String, county: String, district: String)

case class AdditionalLocation(name: String, system: String, line: String, lat: String, lon: String, nrInterchange: String, location: String, `type`: String, operator: String)

case class ChangeTime(crs: String, fromOperator: String, toOperator: String, time: Int, notes: String)

object Location {

  def readChangeTimes(path: String): List[ChangeTime] = {
    var changeTimes = List.empty[ChangeTime]
    try {
      for (line <- Source.fromFile(path).getLines) {
        println(line)
          val parts = line.split(",")
          val ct = ChangeTime(
            parts(0),
            parts(1),
            parts(2),
            parts(3).toInt,
            if(parts.size > 4) parts(4).trim else ""
          )
          changeTimes = ct :: changeTimes
        }
    }
    catch {
      case _: FileNotFoundException => System.err.println("Could not read processed Tiploc file from NR Data... path=" + path)
    }
    changeTimes
  }

  def readAdditionalLocations(path: String): List[AdditionalLocation] = {
    var locations: List[AdditionalLocation] = List.empty[AdditionalLocation]
    try {
      for (line <- Source.fromFile(path).getLines) {
        println(line)
        if (!line.startsWith("Station")) {
          val parts = line.split(",")
          val loc = AdditionalLocation(
            parts(0),
            parts(2),
            parts(3),
            parts(4).toDouble.toString,
            parts(5).toDouble.toString,
            parts(6),
            parts(7),
            parts(8),
            parts(9)
          )
          locations = loc :: locations
        }
      }
    }
    catch {
      case _: FileNotFoundException => System.err.println("Could not read processed Tiploc file from NR Data... path=" + path)
    }
    locations
  }
  def readProcessedStations(path: String): Map[String, EnrichedLocation] = {
    var json: String = ""
    var data: List[EnrichedLocation] = List.empty
    implicit val formats = DefaultFormats
    try {
      json = Source.fromFile(path).mkString
      data = parse(json).extract[List[EnrichedLocation]]
    }
    catch {
      case _: FileNotFoundException => System.err.println("Could not read processed stations file... path=" + path)
    }
    data.map {
      d => (d.id, d)
    }.toMap
  }

  def readProcessedTtisf(file: String): List[TTISFLocation] = {
    var stations: List[TTISFLocation] = List.empty[TTISFLocation]
    try {
      for (line <- Source.fromFile(file).getLines) {
        println(line)
        if (!line.startsWith("//") && line.startsWith("A")) {
          val parts = line.split(",")
          val len = parts.length
          val subsidiaryCrsMaybe = line.substring(49, 56)
          var subsidiaryCrs = ""
          subsidiaryCrsMaybe.toList.foreach {
            c =>
              if (c.isDigit) ()
              else
                subsidiaryCrs = subsidiaryCrs + c
          }
          val category = line.substring(35, 36) match {
            case "0" => "0 - Not an interchange point"
            case "1" => "1 - Small interchange point"
            case "2" => "2 - Medium interchange point"
            case "3" => "3 - Large interchange point"
            case "9" => "9 - Subsidiary TIPLOC for station"

          }
          val loc = TTISFLocation(
            line.substring(5, 35),
            category,
            line.substring(36, 43),
            line.substring(43, 46),
            subsidiaryCrs,
            "",
            false,
            "",
            "",
            Some(parts(len - 2)),
            Some(parts(len - 1))
          )
          stations = loc :: stations
        }
      }
    }
    catch {
      case _: FileNotFoundException => System.err.println("Could not read processed Tiploc file from NR Data... path=" + file)
    }
    stations
  }

  def readFromTtisf(file: String): List[TTISFLocation] = {
    var stations: List[TTISFLocation] = List.empty[TTISFLocation]
    try {
      for (line <- Source.fromFile(file).getLines) {
        println(line)
        if (!line.startsWith("//") && line.startsWith("A")) {
          val loc = TTISFLocation(
            line.substring(5, 35),
            line.substring(35, 36),
            line.substring(36, 43),
            line.substring(43, 46),
            line.substring(49, 52),
            line.substring(52, 57),
            line.substring(57, 58).equals("E"),
            line.substring(58, 63),
            line.substring(63, 65),
            None,
            None
          )
          stations = loc :: stations
        }
      }
    }
    catch {
      case _: FileNotFoundException => System.err.println("Could not read Tiploc file from NR Data... path=" + file)
    }
    stations
  }


  def readStationCodes(file: String): Set[String] = {

    var stations = List.empty[String]
    try {
      for (line <- Source.fromFile(file).getLines) {
        if (!line.startsWith("//")) {
          val parts = line.split(",")
          stations = parts(1) :: stations
        }
      }
    }
    catch {
      case _: FileNotFoundException => System.err.println("Could not read station codes... path=" + file)
    }
    stations.toSet
  }

  def readStationsFromJson(file: String): Map[String, RichStationWithType] = {
    var json: String = ""
    var data: RichStations = RichStations(List.empty[RichStation])
    implicit val formats = DefaultFormats
    try {
      for (line <- io.Source.fromFile(file).getLines) json += line
      data = parse(json).extract[RichStations]
    } catch {
      case _: FileNotFoundException => System.err.println("Could not read station codes... path=" + file)
    }
    data.locations map {
      l =>
        (l.crs, RichStationWithType(
          l.lat,
          l.lon,
          l.crs,
          l.name,
          l.`type`.getOrElse("Station")
        ))
    } toMap

  }

  def readStationsFromSourceJson(stationJsonPath: String): Map[String, Location] = {
    var json: String = ""
    var data: Locations = Locations(List.empty[Location])
    implicit val formats = DefaultFormats
    try {
      for (line <- io.Source.fromFile(stationJsonPath).getLines) json += line
      data = parse(json).extract[Locations]
    } catch {
      case _: FileNotFoundException => System.err.println("Could not read station codes... path=" + stationJsonPath)
      case e: Exception => System.err.println("Something went wrong" + e.getMessage)
    }
    data.locations.map {
      location =>
        (location.tiploc, location)
    }.toMap
  }


  def readStationsFrom17Usage(file: String): Map[String, Estimate17StationEntry] = {
    val stations = new mutable.HashSet[Estimate17StationEntry]
    try {
      for (line <- Source.fromFile(file).getLines) {
        if (!line.startsWith("//")) {
          println(line)
          val parts = line.split(",")
          val num = try {
            parts(16).toLong
          } catch {
            case e: Exception => 0
          }

          stations += Estimate17StationEntry(parts(1), parts(2), parts(3), parts(4), parts(5), parts(6).toLong, parts(7).toLong, parts(8), parts(12), parts(13), parts(14), parts(15), num)
        }
      }
    } catch {
      case _: FileNotFoundException => System.err.println("Could not read station codes... path=" + file)
    }
    stations.map {
      station =>
        (station.tlc, station)
    }.toMap
  }


  def readStationsFrom11Usage(file: String): Map[String, Estimate11StationEntry] = {
    val stations = new mutable.HashSet[Estimate11StationEntry]
    try {
      for (line <- Source.fromFile(file).getLines) {
        if (!line.startsWith("//")) {
          val parts = line.split(",")
          stations += Estimate11StationEntry(parts(1), parts(2), parts(3), parts(4), parts(5), parts(6))
        }
      }
    } catch {
      case _: FileNotFoundException => System.err.println("Could not read station codes... path=" + file)
    }
    stations
      .map {
        station =>
          (station.tlc, station)
      }
      .toMap
  }

}