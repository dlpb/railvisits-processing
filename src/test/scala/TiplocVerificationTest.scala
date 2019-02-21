import java.io.{File, PrintWriter}

import model.EnrichedLocation
import org.json4s.DefaultFormats
import org.json4s.native.JsonMethods.parse
import org.scalatest.{FlatSpec, Matchers}
import timetable.model.Route

import scala.collection.immutable
import scala.concurrent.duration.Duration
import scala.concurrent.forkjoin.ForkJoinPool
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.io.Source

class TiplocVerificationTest extends FlatSpec with Matchers{

  ignore should "work" in {
    val start = System.currentTimeMillis()
    val timetablesFiles:List[File] = new File("""C:\Users\danie\Documents\processed""").listFiles().toList

    //import scala.concurrent.ExecutionContext.Implicits.global
    implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(new ForkJoinPool(32))
    val emptySet: Set[String] = Set.empty

    val futures = timetablesFiles map {
      file: File =>
        Future({
          println(s"$file - START")

          val timetable = readTimetable(file)
          println(s"$file - READ DONE")
          val tiplocs: List[Set[String]] = timetable map {
            t =>
              Set(t.origin.tiploc.trim, t.destination.tiploc.trim) ++ t.intermediatePoints.filter(p =>
                p.activity.equals("T")
                  || p.activity.equals("R")
                  || p.activity.equals("U")
                  || p.activity.equals("D")
              ).map(_.tiploc.trim).toSet
          }
          println(s"$file - MAPPED")
          val tiplocsSet = tiplocs.foldLeft(emptySet)((a: Set[String], b: Set[String]) => a ++ b)
          println(s"$file = ${tiplocsSet.size}")
          tiplocsSet
        })
    }

    val future = Future.sequence(futures)

    val locations = readLocations("C:\\Users\\danie\\Documents\\code\\railvisits\\data\\locations.json")

    println(locations.size)
    val result: immutable.Seq[Set[String]] = Await.result(future, Duration.Inf)
    val tiplocs = result.foldLeft(emptySet)((a: Set[String], b: Set[String]) => a ++ b)
//
    val matches = processTiplocs(tiplocs, locations.filter(_.orrStation))
    writeJson(matches, """C:\Users\danie\Documents\processed\matched.json""")

    println("Result:" + tiplocs.size)
    println(s"Matched ${matches.matched.size}")
    matches.matched.map {m => m.name}.toList.sorted.foreach {println}
    println("---")
    println(s"Unmatched ${matches.unmatched.size}")
    matches.unmatched foreach {
      u =>
        println(locations.find(_.tiploc.equals(u)).map {_.name}.getOrElse(u))
    }
    val end = System.currentTimeMillis()
    val total = end - start
    println(s"took  $total ms")
  }

  def writeJson(content: Matches, path: String): Unit ={
    import org.json4s.native.Serialization.writePretty

    implicit val formats = DefaultFormats

    val jsonString: String = writePretty(content).replaceAllLiterally("\\\"", "")
    new PrintWriter(path) {
      write(jsonString)
      close
    }
  }

  case class Matches(matched: Set[EnrichedLocation], unmatched: Set[String])

  def processTiplocs(tiplocs: Set[String], locations: List[EnrichedLocation]): Matches ={
    def process(tiplocs: Set[String], locations: List[EnrichedLocation], matches: Matches): Matches = {
      if(tiplocs.nonEmpty){
        val tiploc = tiplocs.head
        val matchedLocation = locations.filter {l => l.tiploc.equals(tiploc) || l.tiploc.contains(tiploc)}
        if(matchedLocation.nonEmpty){
          if(matchedLocation.size > 1){
            println("Matched more than 1")
            println(matchedLocation)
          }
          process(tiplocs.tail, locations, Matches(matchedLocation.toSet ++ matches.matched, matches.unmatched))
        }
        else {
          process(tiplocs.tail, locations, Matches(matches.matched, (tiploc :: matches.unmatched.toList).toSet))
        }
      }

      else {
        matches
      }
    }
    process(tiplocs, locations, Matches(Set.empty[EnrichedLocation], Set.empty[String]))
  }

  def readLocations(path: String): List[EnrichedLocation] = {
    var json: String = ""
    implicit val formats = DefaultFormats
    var data: List[EnrichedLocation] = List.empty

    try{
      json = Source.fromFile(path).mkString
      data = parse(json).extract[List[EnrichedLocation]]
    }
    catch {
      case e: Exception => {
        e.printStackTrace()
      }
    }
    data
  }

  def readTimetable(file: File): List[Route] ={
    var json: String = ""
    implicit val formats = DefaultFormats
    var data: List[Route] = List.empty

    try{
      json = Source.fromFile(file.getAbsolutePath).mkString
      data = parse(json).extract[List[Route]]
    }
    catch {
      case e: Exception => {
        e.printStackTrace()
      }
    }
    data

  }
}
