package stations

import java.io.{File, PrintWriter}

import org.json4s.DefaultFormats
import model._

import scala.collection.immutable

object StationEnhancer {
  def stationUsageCSVPath = "../raw-data/Station use - 2011-12.csv"

  def stationJsonPath = "C:\\Users\\danie\\Documents\\code\\raw-data\\station_input.json"

  def stationUsage17CSVPath = "../raw-data/estimates-of-station-usage-2017-18 - Estimates of Station Usage(3).csv"

  def output = "../data/static/locations.json"

  def crsInput = "../raw-data/station_codes(1).csv"

  def crsOutput = "../data/crsOutput.json"

  def tiploc = "../raw-data/enrichedTiplocs.json"

  def ttisfInfo = "../raw-data/ttis144/ttisf144.msn"

  def processedTtisf = "../raw-data/processed.ttisf"

  def additionalLocs = List(
    "../raw-data/Additional Stations - TW.csv",
    "../raw-data/Additional Stations - blackpool.csv",
    "../raw-data/Additional Stations - DLR.csv",
    "../raw-data/Additional Stations - edb.csv",
    "../raw-data/Additional Stations - Heritage.csv",
    "../raw-data/Additional Stations - london tramlink.csv",
    "../raw-data/Additional Stations - LU.csv",
    "../raw-data/Additional Stations - nottingham.csv",
    "../raw-data/Additional Stations - sheffield.csv",
    "../raw-data/Additional Stations - SPT.csv",
    "../raw-data/Additional Stations - Birmingham.csv"
  )


}

class StationEnhancer(
                       stationUsageCSVPath: String,
                       stationJsonPath: String,
                       stationUsage17CSVPath: String,
                       output: String,
                       tiploc: String,
                       crsInput: String,
                       crsOutput: String,
                       ttisfInfo: String,
                       processedTtisf: String,
                       additionalLocations: List[String]) {

  println(new File("../raw-data").listFiles().toList)

  val stations17Enhanced: Map[String, Estimate17StationEntry] = Location.readStationsFrom17Usage(stationUsage17CSVPath)
  val stations: Map[String, Location] = Location.readStationsFromSourceJson(stationJsonPath)
  val stations11Enhanced: Map[String, Estimate11StationEntry] = Location.readStationsFrom11Usage(stationUsageCSVPath)
  val tiplocs: Map[String, RichTiploc] = TiplocLocation.readTiplocFromJson(tiploc)
  val ttisfs = Location.readFromTtisf(ttisfInfo)
  val locationAndTtisfs = Location.readProcessedTtisf(processedTtisf)
  val additionalLocs = additionalLocations.flatMap( path => Location.readAdditionalLocations(path))

  val stationCodes: Set[String] = Location.readStationCodes(crsInput)
    enrich()

  def prettifyLoc(l: EnrichedLocation): EnrichedLocation = {

    def capitalise(s: String) = {
      raw"\b((?<!\b')\w+)".r.replaceAllIn(s.toLowerCase, _.group(1).capitalize)
    }
    EnrichedLocation(
      l.id.replaceAll("\\.", "").trim,
      capitalise(l.name.trim),
      l.operator.trim,
      l.`type`.trim,
      l.location,
      l.nrInfo,
      l.orrStation,
      l.crs.map {_.replaceAll("\\.","").trim },
      l.tiploc.map {_.replaceAll("\\.", "").trim }
    )
  }

  def enrich(): Unit ={
    val locations = StationsV2.convertLocationToEnrichedLocation(stations)
    val enriched: immutable.Seq[EnrichedLocation] = locations
    .withTiplocInformation(tiplocs)
      .withNationalRailLocationInfo(locationAndTtisfs)
      .withORRStationDefinitions(allValidStations().toList)
      .withORR2017Data(stations17Enhanced)
      .withOrr2011Data(stations11Enhanced)
      .withAdditionalLocations(additionalLocs)
      .locations.values.toList map {l => prettifyLoc(l)}



    import org.json4s.native.Serialization.writePretty

    implicit val formats = DefaultFormats

    val jsonString: String = writePretty(enriched).replaceAllLiterally("/\"", "")
    new PrintWriter(output) {
      write(jsonString)
      close
    }
  }


  def allValidStations(): Set[String] = {
    //whats in crs that is not in 1718 stats
    val stats1718: Set[String] = stations17Enhanced.keySet
    stationCodes.intersect(stats1718) ++ Set("HXX", "CFC", "MNS", "KNW", "ILN", "LMR", "CMB").filterNot(_ == "LHR")

  }

//  def old() {
//
//    def unique(ls: List[EnrichedLocation]) = {
//      def loop(set: Set[String], ls: List[EnrichedLocation]): List[EnrichedLocation] = ls match {
//        case head :: tail if set contains head.crs => loop(set, tail)
//        case head :: tail => head :: loop(set + head.crs, tail)
//        case Nil => Nil
//      }
//
//      loop(Set(), ls)
//    }
//
//    val stats1718: _root_.scala.Predef.Set[_root_.scala.Predef.String] = allValidStations
//
//    val enriched: List[EnrichedLocation] = enrich()
//    val enrichedCrs: List[EnrichedLocation] = unique(enriched.filter { l => stationCodes.contains(l.crs) })
//
//    val enrichedCrsCodes: Set[String] = enrichedCrs.map { l => l.crs }.toSet
//
//    val validCrsCodes: Set[String] = enrichedCrsCodes.intersect(stats1718) ++ Set("HXX", "CFC", "MNS", "KNW", "ILN", "LMR", "CMB").filterNot(_ == "LHR")
//    val crsOutputEnriched = validCrsCodes map { crs => enriched.find(l => l.crs == crs).get }
//
//
//    import org.json4s.native.Serialization.writePretty
//
//    implicit val formats = DefaultFormats
//
//    val jsonString: String = writePretty(enriched).replaceAllLiterally("/\"", "")
//    new PrintWriter(output) {
//      write(jsonString)
//      close
//    }
//
//    val crsJsonString: String = writePretty(crsOutputEnriched).replaceAllLiterally("/\"", "")
//    new PrintWriter(crsOutput) {
//      write(crsJsonString)
//      close
//    }
//
//
//
//    def enrich(): List[EnrichedLocation] = {
////      var processedTiplocs: List[String] = List("")
////      var processedCrs: List[String] = List("")
////      var processedTtisfs: List[String] = List("")
////
////      def enrichLocation(tiploc: RichTiploc, location: Location) = {
////        val crs = location.crs
////
////        val srs = if (location.name.contains("(Bus)")) {
////          "ZBUS"
////        } else {
////          location.toc match {
////            case "ES" => "A.15"
////            case "ZB" => "ZBUS"
////            case "ZF" => "ZFERRY"
////            case "LT" => "ZLT"
////            case "LU" => "ZLU"
////            case "TW" => "TW"
////            case _ => "???"
////          }
////        }
////
////        val ttisf = ttisfs.find(t => t.tiploc.equalsIgnoreCase(location.tiploc))
////
////        val matching: List[String] = ttisfs.filter(p => p.crs.equalsIgnoreCase(crs)) map {
////          _.subsidiaryCrs
////        }
////
////        val interchangeType: String = ttisf match {
////          case Some(t) => t.categoryType
////          case _ => "0 - No Interchange"
////        }
////
////        val changeTime = ttisf match {
////          case Some(t) => t.changeTime
////          case _ => "0"
////        }
////
////        val tocInfo = ttisf match {
////          case Some(t) =>
////            processedTtisfs = t.tiploc :: processedTtisfs
////            if (t.name.contains("mtlk")) "ML"
////            else if (t.name.contains("metro")) "TW"
////            else if (t.name.contains(" Bus")) "ZBUS"
////            else location.toc
////          case _ => location.toc
////        }
////
////        val processedTtisf = locationAndTtisfs.find(t => t.tiploc.equalsIgnoreCase(location.tiploc))
////        val lat: Double = if (location.lat.equals("0.0") && processedTtisf.isDefined) {
////          val ttisf = processedTtisf.get
////          ttisf.lat.getOrElse("0.0").toDouble
////        } else location.lat.toDouble
////
////        val lon: Double = if (location.lon.equals("0.0") && processedTtisf.isDefined) {
////          val ttisf = processedTtisf.get
////          ttisf.lon.getOrElse("0.0").toDouble
////        } else location.lon.toDouble
////
////
////        val locType = location.`type`.getOrElse("Station")
////
////        val stations11: Estimate11StationEntry = stations11Enhanced.getOrElse(crs,
////          Estimate11StationEntry("", "", "", "", "", ""))
////        val stations17: Estimate17StationEntry = stations17Enhanced.getOrElse(crs,
////          Estimate17StationEntry("", "", "", "", "", tiploc.location.easting.toDouble, tiploc.location.northing.toDouble, "", srs, "", "", "", 0L))
////
////        val r = EnrichedLocation(
////          location.name,
////          location.tiploc,
////          location.crs,
////          tocInfo,
////          locType,
////          SpatialLocation(lat, lon, stations11.county, stations11.district, stations11.location),
////          NrInfo(
////            stations17.crpLine,
////            stations17.nrRoute,
////            stations17.srsCode),
////          allStationCodes.contains(location.crs) && !processedCrs.contains(location.crs),
////          changeTime,
////          interchangeType,
////          matching.toSet
////        )
////        if (allStationCodes.contains(location.crs)) {
////          processedCrs = location.crs :: processedCrs
////        }
////
////        r
////      }
//
////      val richTiplocs: List[EnrichedLocation] = tiplocs.keySet.map {
////        t =>
////          processedTiplocs = t :: processedTiplocs
////          val tiploc = tiplocs(t)
////          val location = stations.getOrElse(t,
////            Location(tiploc.location.lat, tiploc.location.lon, t, tiploc.name, t, tiploc.location.toc, tiploc.location.`type`))
////          val r: EnrichedLocation = enrichLocation(tiploc, location)
////          println(r)
////          r
////
////      }.toList.sortWith((a, b) => {
////        val srs = a.nrInfo.srs.compareTo(b.nrInfo.srs)
////        val toc = a.toc.compareTo(b.toc)
////        val name = a.name.compareTo(b.name)
////        srs != 0 || toc != 0 || name != 0
////      })
////
////      val richStations: List[EnrichedLocation] = stations.keySet.filter({
////        key =>
////          !processedTiplocs.contains(key)
////      }).map {
////        loc =>
////          val location = stations(loc)
////          val richTiploc = RichTiploc(location.tiploc, location.crs, location.name, TiplocLocation(location.lat.toString, location.lon.toString, "0", "0", location.toc, location.`type`))
////          processedCrs = richTiploc.crs :: processedCrs
////          enrichLocation(richTiploc, location)
////      }.toList
////
////      val enrichedTtisfs = locationAndTtisfs.filterNot(l => processedCrs.contains(l.crs)) map {
////        t =>
////          val matching: List[String] = ttisfs.filter(p => p.crs.equalsIgnoreCase(t.crs)) map {
////            _.subsidiaryCrs
////          }
////
////          val latStr = t.lat.getOrElse("0.0")
////          val lonStr = t.lon.getOrElse("0.0")
////
////          val lat = (if (latStr.isEmpty) "0.0" else latStr).toDouble
////          val lon = (if (lonStr.isEmpty) "0.0" else lonStr).toDouble
////
////          EnrichedLocation(t.name, t.tiploc, t.crs, "XX", "Location",
////            SpatialLocation(lat, lon, "", "", ""),
////            NrInfo("", "", ""), false, t.changeTime, t.categoryType, matching.toSet)
////      }
////      val locs = richTiplocs ::: richStations ::: enrichedTtisfs sortWith ((a, b) => a.name < b.name)
////
////          println("Need to research")
////          var count = 0
//
//      //   val richTtisfs: List[TTISFLocation] = ttisfs.map {
//      //      ttisf =>
//      //
//      //        val richTiploc = locs.find(t => ttisf.tiploc == t.tiploc)
//      //
//      //        if (richTiploc.isDefined) {
//      //          val loc = richTiploc.get
//      //          TTISFLocation(ttisf.name, ttisf.categoryType, ttisf.tiploc, ttisf.subsidiaryCrs, loc.crs, loc.location.oseast.toString, false, loc.location.osnorth.toString, ttisf.changeTime, Some(loc.location.lat.toString), Some(loc.location.lon.toString))
//      //        }
//      //        else {
//      //          val byName = richTiplocs.find(t => t.name.toUpperCase().trim().equalsIgnoreCase(ttisf.name.toUpperCase().trim()))
//      //          if(byName.isDefined){
//      //            val loc = byName.get
//      //            TTISFLocation(ttisf.name, ttisf.categoryType, ttisf.tiploc, ttisf.subsidiaryCrs, loc.crs, loc.location.oseast.toString, false, loc.location.osnorth.toString, ttisf.changeTime, Some(loc.location.lat.toString), Some(loc.location.lon.toString))
//      //          }
//      //          else {
//      //            count = count + 1
//      //            ttisf
//      //          }
//      //        }
//      //    }
//      //
//      //    println(count)
//      //
//      //
//      //    new PrintWriter(output+".ttisf") {
//      //      write(richTtisfs.mkString(System.lineSeparator()))
//      //      close
//      //    }
//      //
//      //
//
//
////      locs
////    }
//      ???
//  }


}

object Main extends App {
  new StationEnhancer(
    StationEnhancer.stationUsageCSVPath,
    StationEnhancer.stationJsonPath,
    StationEnhancer.stationUsage17CSVPath,
    StationEnhancer.output,
    StationEnhancer.tiploc,
    StationEnhancer.crsInput,
    StationEnhancer.crsOutput,
    StationEnhancer.ttisfInfo,
    StationEnhancer.processedTtisf,
    StationEnhancer.additionalLocs
  )
}


