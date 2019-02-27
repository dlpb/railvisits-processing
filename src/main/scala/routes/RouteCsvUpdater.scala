package routes

package routes

import java.io._

import model._
import org.json4s.DefaultFormats

import scala.collection.mutable
import scala.io.Source

/**
  * Created by Daniel on 24/11/2018.
  */
class RouteCsvUpdater(graphPath: String, jsonPath: String, output: String) {

  var lines: String = ""
  val processedStations: Iterable[EnrichedLocation] = Location.readProcessedStations(jsonPath).values
  val connection = new mutable.HashSet[GraphConnection]
  try {
    for (line <- Source.fromFile(graphPath).getLines) {
      val parts = line.split(",")
      println(line)
      val newLine = replaceNamesWithIds(line, parts)
      println(newLine)

      lines = lines + "\n" + newLine


    }
    val writer = new BufferedWriter(new FileWriter(new File(output)))
    writer.write(lines)
    writer.close()
  }
  catch {
    case _: FileNotFoundException => System.err.println("Could not read graph file... Path: " + graphPath)
  }


  def getStationByName(name: String): Option[EnrichedLocation] = {
    processedStations.find(_.name.equals(name))
  }

  def replaceNamesWithIds(str: String, parts: Array[String]): String = {
    val from = getStationByName(parts(0))
    val to = getStationByName(parts(1))

    val fromId = from match {
      case Some(station) => station.id
      case None => parts(0)
    }

    val toId = to match {
      case Some(station) => station.id
      case None => parts(1)
    }

    val restOfParts = parts.drop(2)
    fromId +: toId +: restOfParts :+ "" :+ "" mkString ","
  }

}

object RouteCsvUpdater {

  def graphFilePath = "../raw-data/RAIL ROUTE GRAPH - Sheet1(1).csv"

  def stationJsonPath = "C:\\Users\\danie\\Documents\\code\\data\\static\\locations.json"

  def output = "../raw-data/RAIL ROUTE GRAPH - Sheet1(1)-2.csv"


}

object Main extends App {
  new RouteCsvUpdater(
    RouteCsvUpdater.graphFilePath,
    RouteCsvUpdater.stationJsonPath,
    RouteCsvUpdater.output
  )
}

