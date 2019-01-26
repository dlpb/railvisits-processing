package model

import java.io.FileNotFoundException

import scala.collection.mutable
import scala.io.Source

case class Station(crs: String)

object Station{
  def readStationCodes(file: String): Set[Station] = {
    val stations = new mutable.HashSet[Station]
    try {
      for (line <- Source.fromFile(file).getLines) {
        val parts = line.split(",")
        stations += Station(parts(1))
      }
    }
    catch {
      case _: FileNotFoundException => System.err.println("Could not read station codes... Path=" + file)
    }
    stations.toSet
  }
}