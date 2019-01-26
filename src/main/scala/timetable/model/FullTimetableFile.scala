package timetable.model

import java.io.{FileWriter, PrintWriter}

import org.json4s.DefaultFormats
import timetable.model.Main.{outputFolder}

import scala.io.Source

class FullTimetableFile {

}

object Tocs {
  def tocs: Set[String] = Set("HT", "LN", "EM", "IL", "TW", "ME", "ES", "FC", "ZZ", "VT", "SW", "LM", "SP", "GC", "LT", "CC", "CH", "LE", "SE", "AR", "HC", "GX", "SJ", "LS", "GW", "TP", "CS", "GN", "HX", "LO", "GR", "TL", "AW", "XC", "SN", "NT", "XR", "NY", "LR", "SR")
}

object Main extends App {

  val input = """C:\Windows\Temp\ttis144\ttisf144.mca"""
  val output = """C:\Windows\Temp\ttis144\\processed\timetable.json"""
  val outputFolder = """C:\Windows\Temp\ttis144\\processed\"""




    val data = Source.fromFile(input).getLines()
//  val data =
//    "AANG17089G807681911021912140000010JJSCRNLRCH  TP                               P\n" +
//      "BSNC108651812191905171110100 PXX1S531220122180012 DMUV   125      B S T        P\n" +
//      "BX         XCYXC122000                                                          \n" +
//      "LOPLYMTH  1325 13257         TB                                                 \n" +
//      "LITOTNES  1349H1351      135013512        T              H                      \n" +
//      "CRBHAMNWS XX1S531220122180008 DMUV   125      B S T                XC122000     \n" +
//      "LTEDINBUR 2213 221319 NL TF                                                     \n" +
//      "BSNC108661812191905170111100 PXX1P004780122180013 DMUE   100      B S          P\n" +
//      "BX         XCYXC478000                                                          \n" +
//      "LOBRSTLTM 0520 05203  UF     TB                                                 \n" +
//      "LIBRSTPWY 0529 0538      052905384        T RM                                  \n" +
//      "LTCRDFCEN 0623 06230     TF                                                     "


  val timetable = FullTimetableFile.processData(data, outputFolder)


}

object FullTimetableFile {
  def processData(data: String): List[Route] = List.empty

  def getWriter(writers: Map[String, FileWriter], ATOCCode: String) : FileWriter = {
    val fw: FileWriter = writers.getOrElse(ATOCCode, writers.get("other").get)
    fw
  }
  def makeWriters(tocs: Set[String], output: String): Map[String, FileWriter] = {
    import java.io.FileWriter

    val otherTocs: Set[String] = tocs + "other"

    val writers: Map[String, FileWriter] = otherTocs.map {
      toc =>
        val fw = new FileWriter(outputFolder + toc + ".json", true)
        (toc -> fw)
    }.toMap

    writers

  }

  def processData(lines: Iterator[String], outputPath: String): Unit = {

    import org.json4s.native.Serialization.writePretty

    implicit val formats = DefaultFormats
    val writers = makeWriters(Tocs.tocs, outputPath)
    writers foreach {
      writer =>
        writer._2.write("[")
    }

    var buffer = ""
    var count = 0

    var recordStarted = false
    while (lines.hasNext) {
      val line = lines.next()
      if (line.startsWith("BS") && !recordStarted) {
        recordStarted = true
        buffer = line
      }
      else if (recordStarted &&
        line.startsWith("BX")
        || line.startsWith("LO")
        || line.startsWith("LI")
        || line.startsWith("CR")) {
        buffer = buffer + "\n" + line
      }
      else if (recordStarted && line.startsWith("LT")) {
        buffer = buffer + "\n" + line
        val route = makeRouteFrom(buffer)

        val jsonString: String = writePretty(route).replaceAllLiterally("\\\"", "")
        val fileWriter = getWriter(writers, route.basicScheduleExtraDetail.ATOCCode)
        fileWriter.append(jsonString)
        fileWriter.append(",")
        buffer = ""
        count = count + 1
        recordStarted = false
      }
    }
    writers.foreach {
      writer =>
        writer._2.append("{} ]")
        writer._2.close()
    }

  }

  def processLine(line: String) = Line(line).toRecord

  def makeRouteFrom(data: String): Route = {
    var points = List.empty[IntermediateLocationRecord]
    var basicInfo: BasicScheduleRecord = null
    var extraInfo: BasicScheduleExtraDetailsRecord = null
    var origin: OriginLocationRecord = null
    var destination: TerminatingLocationRecord = null

    data.lines foreach {
      line =>
        val record = processLine(line)
        record match {
          case r: BasicScheduleRecord => basicInfo = r
          case r: BasicScheduleExtraDetailsRecord => extraInfo = r
          case r: OriginLocationRecord => origin = r
          case r: IntermediateLocationRecord => points = r :: points
          case r: TerminatingLocationRecord => destination = r
          case _ =>
        }
    }

    Route(basicInfo, extraInfo, origin, points.reverse, destination)

  }
}

case class Route(basicScheduleInfo: BasicScheduleRecord,
                 basicScheduleExtraDetail: BasicScheduleExtraDetailsRecord,
                 origin: OriginLocationRecord,
                 intermediatePoints: List[IntermediateLocationRecord],
                 destination: TerminatingLocationRecord)

case class Line(line: String) {


  def toRecord: Record = {
    if (line.length != 80) {
      throw new IllegalArgumentException(s"Line length must be exactly 80. Line length was ${line.length} - '$line'")
    }
    val recordIdentity = line.substring(0, 2)

    recordIdentity match {
      case "BS" =>
        makeBasicRecord()
      case "BX" =>
        makeBasicExtraDataRecord()
      case "LO" =>
        makeLocationOriginRecord()
      case "LI" =>
        makeIntermediateLocationRecord()
      case "LT" =>
        makeTerminalLocationRecord()
      case "AA" =>
        makeAssociationRecord()
      case "CR" =>
        makeChangeEnRouteRecord()
    }
  }

  private def makeChangeEnRouteRecord(): ChangeEnRouteRecord = {
    val recordId = line.substring(0, 2)
    val tiploc = line.substring(2, 9)
    val data = line.substring(9)
    ChangeEnRouteRecord(recordId, tiploc, data)
  }

  private def makeAssociationRecord(): AssociationRecord = {
    val transactionType = line.substring(2, 3)
    val uid = line.substring(3, 9)
    val associatedUid = line.substring(9, 15)
    val start = line.substring(15, 21)
    val end = line.substring(21, 27)
    val days = line.substring(27, 34)
    val category = line.substring(34, 36)
    val dateIndicator = line.substring(36, 37)
    val location = line.substring(37, 44)
    val baseLocSuffix = line.substring(44, 45)
    val locSuffix = line.substring(45, 46)
    val diagramType = line.substring(46, 47)
    val assocType = line.substring(47, 48)
    val stp = line.substring(79, 80)
    AssociationRecord(
      "AA",
      transactionType,
      uid,
      associatedUid,
      start,
      end,
      days.substring(0, 1).equals("1"),
      days.substring(1, 2).equals("1"),
      days.substring(2, 3).equals("1"),
      days.substring(3, 4).equals("1"),
      days.substring(4, 5).equals("1"),
      days.substring(5, 6).equals("1"),
      days.substring(6, 7).equals("1"),
      category,
      dateIndicator,
      location,
      baseLocSuffix,
      locSuffix,
      diagramType,
      assocType,
      stp
    )
  }

  private def makeTerminalLocationRecord(): TerminatingLocationRecord = {
    val tiploc = line.substring(2, 9)
    val wttArr = line.substring(10, 14)
    val wttArrHalf = line.substring(14, 15)
    val wttDep = line.substring(15, 19)
    val platform = line.substring(19, 20)
    TerminatingLocationRecord("LT", tiploc, wttArr, wttArrHalf, wttDep, platform)
  }

  private def makeIntermediateLocationRecord(): IntermediateLocationRecord = {
    val tiploc = line.substring(2, 9)
    val wttArr = line.substring(10, 14)
    val wttHalfArr = line.substring(14, 15)
    val wttDep = line.substring(15, 19)
    val wttHalf = line.substring(19, 20)
    val pass = line.substring(20, 24)
    val publicArr = line.substring(25, 29)
    val publicDep = line.substring(29, 33)
    val platform = line.substring(33, 36)
    val activity = line.substring(42,43)
    IntermediateLocationRecord("LI", tiploc, wttArr, wttHalfArr, wttHalf, wttDep, publicArr, publicDep, platform, pass, activity)
  }


  private def makeLocationOriginRecord(): OriginLocationRecord = {
    val tiploc = line.substring(2, 9)
    val wttArr = line.substring(10, 14)
    val wttArrHalf = line.substring(14, 15)
    val wttDep = line.substring(15, 19)
    val platform = line.substring(19, 20)
    OriginLocationRecord("LO", tiploc, wttArr, wttArrHalf, wttDep, platform)
  }

  private def makeBasicExtraDataRecord(): Record = {
    val tractionClass = line.substring(2, 6)
    val uicCode = line.substring(6, 11)
    val atocCode = line.substring(11, 13)
    val applicableTimetableCode = line.substring(13, 14)
    val rsid = line.substring(14, 22)
    val dataSource = line.substring(22, 23)

    BasicScheduleExtraDetailsRecord(
      "BX",
      tractionClass,
      uicCode,
      atocCode,
      applicableTimetableCode,
      rsid,
      dataSource)
  }

  private def makeBasicRecord(): BasicScheduleRecord = {

    val transactionType = line.substring(2, 3)
    val recordType = transactionType
    val trainUid = line.substring(3, 9)
    val runsFrom = line.substring(9, 15)
    val runsTo = line.substring(15, 21)
    val runs = line.substring(21, 28)
    val bankHolidays = line.substring(28, 29)
    val trainStatus = line.substring(29, 30)
    val trainCategory = line.substring(30, 32)
    val trainIdentity = line.substring(32, 36)
    val headCode = line.substring(36, 40)
    val courseIndicator = line.substring(40, 41)
    val trainServiceCode = line.substring(41, 49)
    val portionId = line.substring(49, 50)
    val powerType = line.substring(50, 53)
    val timingLoad = line.substring(53, 57)
    val speed = line.substring(57, 60)
    val operatingCharacteristics = line.substring(60, 66)
    val seatingClass = line.substring(66, 67)
    val sleepers = line.substring(67, 68)
    val reservations = line.substring(68, 69)
    val connectionIndicator = line.substring(69, 70)
    val cateringCode = line.substring(70, 74)
    val serviceBranding = line.substring(74, 78)
    val spare = line.substring(78, 79)
    val stpIndicator = line.substring(79, 80)

    BasicScheduleRecord(
      "BS",
      recordType,
      trainUid,
      runsFrom,
      runsTo,
      runs.substring(0, 1).equals("1"),
      runs.substring(1, 2).equals("1"),
      runs.substring(2, 3).equals("1"),
      runs.substring(3, 4).equals("1"),
      runs.substring(4, 5).equals("1"),
      runs.substring(5, 6).equals("1"),
      runs.substring(6, 7).equals("1"),
      !bankHolidays.equals("X"),
      !bankHolidays.equals("G"),
      trainStatus,
      trainCategory,
      trainIdentity,
      headCode,
      courseIndicator,
      trainServiceCode,
      portionId,
      powerType,
      timingLoad,
      speed,
      operatingCharacteristics,
      seatingClass,
      sleepers,
      reservations,
      connectionIndicator,
      cateringCode,
      serviceBranding,
      stpIndicator
    )
  }
}

sealed trait Record

sealed trait TiplocRecord extends Record

sealed trait TrainScheduleRecord extends Record

case class HeaderRecord(line: String) extends Record

case class TiplocInsertRecord(line: String) extends TiplocRecord

case class TiplocDeleteRecord(line: String) extends TiplocRecord

case class TiplocAmendRecord(line: String) extends TiplocRecord

case class AssociationRecord(identity: String,
                             transactionType: String,
                             mainTrainUid: String,
                             associatedTrainUid: String,
                             startDate: String,
                             endDate: String,
                             validMonday: Boolean,
                             validTuesday: Boolean,
                             validWednesday: Boolean,
                             validThursday: Boolean,
                             validFriday: Boolean,
                             validSaturday: Boolean,
                             validSunday: Boolean,
                             category: String,
                             dateIndicator: String,
                             location: String,
                             baseLocationSuffix: String,
                             locationSuffix: String,
                             diagramType: String,
                             associationType: String,
                             stpIndicator: String) extends Record

case class BasicScheduleRecord(identity: String,
                               transactionType: String,
                               trainUid: String,
                               dateFrom: String,
                               dateTo: String,
                               runsMonday: Boolean,
                               runsTuesday: Boolean,
                               runsWednesday: Boolean,
                               runsThursday: Boolean,
                               runsFriday: Boolean,
                               runsSaturday: Boolean,
                               runsSunday: Boolean,
                               runsBankHoliday: Boolean,
                               runsGlasgowBankHoliday: Boolean,
                               trainStatus: String,
                               trainCategory: String,
                               trainIdentity: String,
                               headCode: String,
                               courseIndicator: String,
                               trainServiceCode: String,
                               portionId: String,
                               powerType: String,
                               timingLoad: String,
                               speed: String,
                               operatingCharacteristics: String,
                               seatingClass: String,
                               sleepers: String,
                               reservations: String,
                               connectionIndicator: String,
                               cateringCode: String,
                               serviceBranding: String,
                               stpIndicator: String
                              ) extends TrainScheduleRecord

case class BasicScheduleExtraDetailsRecord(
                                            recordIdentity: String,
                                            tractionClass: String,
                                            UICCode: String,
                                            ATOCCode: String,
                                            ApplicableTimetableCode: String,
                                            RSID: String,
                                            dataSource: String
                                          ) extends TrainScheduleRecord

case class TrainSpecificNoteRecord(line: String) extends TrainScheduleRecord

case class OriginLocationRecord(
                                 recordIdentity: String,
                                 tiploc: String,
                                 wttArr: String,
                                 wttHalfMinuteArrival: String,
                                 wttDep: String,
                                 platform: String
                               ) extends TrainScheduleRecord


case class IntermediateLocationRecord(recordIdentity: String,
                                      tiploc: String,
                                      wttArr: String,
                                      wttHalfMinuteArrival: String,
                                      wttHalfMinuteDeparture: String,
                                      wttDep: String,
                                      pubArr: String,
                                      pubDep: String,
                                      platform: String,
                                      pass: String,
                                      activity: String
                                     ) extends TrainScheduleRecord {

}

case class ChangeEnRouteRecord(recordIdentity: String,
                               tiploc: String,
                               data: String
                              ) extends TrainScheduleRecord

case class TerminatingLocationRecord(recordIdentity: String,
                                     tiploc: String,
                                     wttArr: String,
                                     wttHalfMinuteArrival: String,
                                     wttDep: String,
                                     platform: String
                                    ) extends TrainScheduleRecord

case class TrailerRecord(line: String)


