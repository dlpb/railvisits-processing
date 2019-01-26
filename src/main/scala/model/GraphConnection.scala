package model

import java.io.FileNotFoundException

import scala.collection.mutable
import scala.io.Source

case class RichGraphConnection(from: RichStationWithType, to: RichStationWithType,toc: String, singleTrack: String, electrification: String, speed: String, srsCode: String)
case class GraphConnection(from: String, to: String, toc: String, singleTrack: String, electrification: String, speed: String, srsCode: String)

object GraphConnection {
  def readAdditionalData(file: String): Set[GraphConnection] = {
    val connection = new mutable.HashSet[GraphConnection]
    try {
      for (line <- Source.fromFile(file).getLines) {
      println(line)
        if(line.length > 3){
          val parts = line.split(" ")
          connection += GraphConnection(parts(4), parts(6), parts(2), "NA", "NA", parts(9) + " " + parts(10), "Link")
        }
      }
    }
    catch {
      case _: FileNotFoundException => System.err.println("Could not read additional links from national rail data download... Path: " + file)
    }
    connection.toSet
  }

  def readGraph(file: String): Set[GraphConnection] = {
    val connection = new mutable.HashSet[GraphConnection]
    try {
      for (line <- Source.fromFile(file).getLines) {
        val parts = line.split(",")
        println(line)
        connection += GraphConnection(parts(0), parts(1), parts(2), parts(3), parts(4), parts(5), parts(6))
      }
    }
    catch {
      case _: FileNotFoundException => System.err.println("Could not read graph file... Path: " + file)
    }
    connection.toSet
  }

  def readGraphStations(file: String): Set[Station] = {
    val stations = new mutable.HashSet[Station]
    try {
      for (line <- Source.fromFile(file).getLines) {
        println(line)
        val parts = line.split(",")
        stations += Station(parts(0))
        stations += Station(parts(1))
      }
    }
    catch {
      case _: FileNotFoundException => System.err.println("Could not read stations file... Path: " + file)
    }
    stations.toSet
  }
}
