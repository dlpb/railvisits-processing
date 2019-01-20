import org.scalatest.{FlatSpec, Matchers}
import timetable.model._

class FullTimetableTest extends FlatSpec with Matchers {

  it should "convert a basic record line into a basic record object" in {
    val recordStr = "BSNC910211812191905171111100 POO2U10    125530004 DMUN   075      S            P"

    val result = FullTimetableFile.processLine(recordStr)
    result shouldBe a[BasicScheduleRecord]
    val record = result.asInstanceOf[BasicScheduleRecord]

    record.identity should be("BS")
    record.transactionType should be(New)
    record.trainUid should be("C91021")
    record.dateFrom should be("181219")
    record.dateTo should be("190517")
    record.runsMonday should be(true)
    record.runsTuesday should be(true)
    record.runsWednesday should be(true)
    record.runsThursday should be(true)
    record.runsFriday should be(true)
    record.runsSaturday should be(false)
    record.runsSunday should be(false)
    record.runsBankHoliday should be(true)
    record.runsGlasgowBankHoliday should be(true)
    record.trainStatus should be("P")
    record.trainCategory should be("OO")
    record.trainIdentity should be("2U10")
    record.headCode should be("    ")
    record.courseIndicator should be("1")
    record.trainServiceCode should be("25530004")
    record.portionId should be(" ")
    record.powerType should be("DMU")
    record.timingLoad should be("N   ")
    record.speed should be("075")
    record.operatingCharacteristics should be("      ")
    record.seatingClass should be("S")
    record.sleepers should be(" ")
    record.reservations should be(" ")
    record.connectionIndicator should be(" ")
    record.cateringCode should be("    ")
    record.serviceBranding should be("    ")
    record.stpIndicator should be("P")

  }
  it should "convert a basic record extra details line" in {
    val line = "BX         CHYCH148900                                                          "
    val result = FullTimetableFile.processLine(line)

    result shouldBe a[BasicScheduleExtraDetailsRecord]
    val record = result.asInstanceOf[BasicScheduleExtraDetailsRecord]

    record.recordIdentity should be("BX")
    record.tractionClass should be("    ")
    record.UICCode should be("     ")
    record.ATOCCode should be("CH")
    record.ApplicableTimetableCode should be("Y")
    record.RSID should be("CH148900")
    record.dataSource should be(" ")
  }

  it should "convert a location origin record line" in {
    val line = "LOMARYLBN 0700 07002         TB                                                 "
    val result = FullTimetableFile.processLine(line)

    result shouldBe a[OriginLocationRecord]

    val record = result.asInstanceOf[OriginLocationRecord]
    record.tiploc should be("MARYLBN")
    record.wttArr should be("0700")
    record.wttHalfMinuteArrival should be(" ")
    record.wttDep should be("0700")
    record.platform should be("2")
  }

  it should "convert a location pass record line" in {
    val line = "LINEASDSJ           0707 00000000                                               "
    val result = FullTimetableFile.processLine(line)

    result shouldBe a[IntermediateLocationRecord]

    val record = result.asInstanceOf[IntermediateLocationRecord]
    record.tiploc should be("NEASDSJ")
    record.pass should be("0707")
  }

  it should "convert a location stop record" in {
    val line = "LINTHOLTP 0715 0716      07150716         T                H                    "
    val result = FullTimetableFile.processLine(line)

    result shouldBe a[IntermediateLocationRecord]

    val record = result.asInstanceOf[IntermediateLocationRecord]
    record.tiploc should be("NTHOLTP")
    record.wttArr should be("0715")
    record.wttHalfMinuteArrival should be(" ")
    record.wttDep should be("0716")
    record.wttHalfMinuteDeparture should be(" ")
    record.pubArr should be("0715")
    record.pubDep should be("0716")
    record.pass should be("    ")
  }

  it should "convert a location stop with half departure record" in {
    val line = "LIWRUISLP 0731 0731H     07310731         T                                     "
    val result = FullTimetableFile.processLine(line)

    result shouldBe a[IntermediateLocationRecord]

    val record = result.asInstanceOf[IntermediateLocationRecord]
    record.tiploc should be("WRUISLP")
    record.wttArr should be("0731")
    record.wttHalfMinuteArrival should be(" ")
    record.wttDep should be("0731")
    record.wttHalfMinuteDeparture should be("H")
    record.pubArr should be("0731")
    record.pubDep should be("0731")
    record.pass should be("    ")
  }

  it should "convert a location stop with half arrival and platform record" in {
    val line = "LIDENHAM  0735H0736      073607361        T                                     "
    val result = FullTimetableFile.processLine(line)

    result shouldBe a[IntermediateLocationRecord]

    val record = result.asInstanceOf[IntermediateLocationRecord]
    record.tiploc should be("DENHAM ")
    record.wttArr should be("0735")
    record.wttHalfMinuteArrival should be("H")
    record.wttDep should be("0736")
    record.wttHalfMinuteDeparture should be(" ")
    record.pubArr should be("0736")
    record.pubDep should be("0736")
    record.pass should be("    ")
    record.platform should be("1  ")
  }

  it should "convert a location ending record line" in {
    val line = "LTBNBR    0845 08454     TF                                                     "
    val result = FullTimetableFile.processLine(line)

    result shouldBe a[TerminatingLocationRecord]

    val record = result.asInstanceOf[TerminatingLocationRecord]
    record.tiploc should be("BNBR   ")
    record.wttArr should be("0845")
    record.wttHalfMinuteArrival should be(" ")
    record.wttDep should be("0845")
    record.platform should be("4")
  }

  it should "process an association record" in {
    val line = "AANL29049L292711812191905171111100VVSCLCHSTR  TP                               P"
    val result = FullTimetableFile.processLine(line)

    result shouldBe a[AssociationRecord]

    val record = result.asInstanceOf[AssociationRecord]
    record.transactionType should be("N")
    record.mainTrainUid should be("L29049")
    record.associatedTrainUid should be("L29271")
    record.startDate should be("181219")
    record.endDate should be("190517")
    record.validMonday should be(true)
    record.validTuesday should be(true)
    record.validWednesday should be(true)
    record.validThursday should be(true)
    record.validFriday should be(true)
    record.validSaturday should be(false)
    record.validSunday should be(false)
    record.category should be("VV")
    record.dateIndicator should be("S")
    record.location should be ("CLCHSTR")
    record.baseLocationSuffix should be(" ")
    record.locationSuffix should be(" ")
    record.diagramType should be("T")
    record.associationType should be("P")
    record.stpIndicator should be("P")

  }

  it should "read in a series of lines and form a route with metadata" in {
    val data =  "BSNC910211812191905171111100 POO2U10    125530004 DMUN   075      S            P\n"+
                "BX         CHYCH148900                                                          \n"+
                "LOMARYLBN 0700 07002         TB                                                 \n"+
                "LINEASDSJ           0707 00000000                                               \n"+
                "LIWEMBLSM 0708H0709      070907092        T                                     \n"+
                "LTBNBR    0845 08454     TF                                                     \n"


    val route = FullTimetableFile.makeRouteFrom(data)

    route.basicScheduleInfo.trainUid should be("C91021")
    route.basicScheduleExtraDetail.ATOCCode should be("CH")
    route.origin.tiploc should be("MARYLBN")
    route.intermediatePoints.size should be(2)
    route.intermediatePoints.head.tiploc should be("NEASDSJ")
    route.intermediatePoints.tail.head.tiploc should be("WEMBLSM")
    route.destination.tiploc should be("BNBR   ")

  }


  ignore should "make routes from data" in {
    val data =
        "AANG17089G807681911021912140000010JJSCRNLRCH  TP                               P\n" +
        "BSNC108651812191905171110100 PXX1S531220122180012 DMUV   125      B S T        P\n" +
        "BX         XCYXC122000                                                          \n" +
        "LOPLYMTH  1325 13257         TB                                                 \n" +
        "LITOTNES  1349H1351      135013512        T              H                      \n" +
        "CRBHAMNWS XX1S531220122180008 DMUV   125      B S T                XC122000     \n" +
        "LTEDINBUR 2213 221319 NL TF                                                     \n" +
        "BSNC108661812191905170111100 PXX1P004780122180013 DMUE   100      B S          P\n" +
        "BX         XCYXC478000                                                          \n" +
        "LOBRSTLTM 0520 05203  UF     TB                                                 \n" +
        "LIBRSTPWY 0529 0538      052905384        T RM                                  \n" +
        "LTCRDFCEN 0623 06230     TF                                                     "

    val routes = FullTimetableFile.processData(data)
    routes.size should be(2)
  }

}

