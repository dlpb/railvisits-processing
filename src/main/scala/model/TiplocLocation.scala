package model

import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse

import scala.collection.mutable
import scala.io.Source

  case class Tiploc(easting: String, northing: String, tiploc: String, name: String, gridRef: String, lat: String, lon: String)

  case class RichTiploc(tiploc: String, crs: String, name: String, location: TiplocLocation)
  case class TiplocLocation(lat: String, lon: String, easting: String, northing: String, toc: String, `type`: Option[String])



object TiplocLocation {
  def readTiplocFromJson(file: String): Map[String, RichTiploc] = {
    implicit val formats = DefaultFormats

    val json = io.Source.fromFile(file).mkString
    val data = parse(json).extract[List[RichTiploc]]
    data.map {
      t =>
        (t.tiploc, t)
    }.toMap
  }
  def readTiplocFromCsv(file: String): Map[String, Tiploc] = {
    val tiplocs = new mutable.HashSet[Tiploc]
    for(line <- Source.fromFile(file).getLines()){
      if(!line.startsWith("//")){
        val parts = line.split(",")
        println(line)
        tiplocs += Tiploc(parts(0), parts(1), parts(2), parts(3), parts(4), parts(5), parts(6))
      }
    }
    tiplocs.map {
      t =>
        (t.tiploc, t)
    }.toMap
  }
}