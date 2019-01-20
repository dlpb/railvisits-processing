package model

import scala.collection.mutable
import scala.io.Source

case class Station(crs: String)

object Station{
  def readStationCodes(file: String): Set[Station] = {
    val stations = new mutable.HashSet[Station]
    for (line <- Source.fromFile(file).getLines) {
      val parts = line.split(",")
      stations += Station(parts(1))
    }
    stations.toSet
  }
}