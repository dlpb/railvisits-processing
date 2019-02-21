import model._
import stations.StationsV2
import org.scalatest.{FlatSpec, Matchers}

class StationsTest extends FlatSpec with Matchers {


  "StationsV2" should "read in a list of stations" in {
    val stationsMap: Map[String, Location] = List("CTH" -> Location("1.2", "2.3", "CHDWLHT", "Chadwell Heath", "CTH", "XR", Some("Station"))) toMap
    val enrichedLocationHolder = StationsV2.convertLocationToEnrichedLocation(stationsMap)
    val enrichedLocation = enrichedLocationHolder.locations

    enrichedLocation.size should be(1)
    enrichedLocation("CTH").location.lat should be(1.2)
    enrichedLocation("CTH").location.lon should be(2.3)
    enrichedLocation("CTH").tiploc should be(Set("CHDWLHT"))
    enrichedLocation("CTH").name should be("Chadwell Heath")
    enrichedLocation("CTH").id should be("CTH")
    enrichedLocation("CTH").crs should be(Set("CTH"))
    enrichedLocation("CTH").operator should be("XR")
    enrichedLocation("CTH").`type` should be("Station")

    enrichedLocation("CTH").location.county should be(None)
    enrichedLocation("CTH").location.district should be(None)
    enrichedLocation("CTH").location.postcode should be(None)
    enrichedLocation("CTH").nrInfo should be(None)
    enrichedLocation("CTH").orrStation should be(false)
  }

  it should "default to a type of Infrasturcture if not specified in stations Json" in {
    val stationsMap: Map[String, Location] = List("CTH" -> Location("1.2", "2.3", "CHDWLHT", "Chadwell Heath", "CTH", "XR", None)) toMap
    val enrichedLocationHolder = StationsV2.convertLocationToEnrichedLocation(stationsMap)
    val enrichedLocation = enrichedLocationHolder.locations

    enrichedLocation.size should be(1)
    enrichedLocation("CTH").`type` should be("Station")
  }

  it should "read in TIPLOC location with same details as station as one location" in {
    val tiplocMap: Map[String, RichTiploc] = List("CHDWLHT" -> RichTiploc("CHDWLHT" , "CTH", "Chadwell Heath", TiplocLocation("not-used", "not-used", "not-used", "not-used", "not-used", Some("not-used")))).toMap
    val stationsMap: Map[String, Location] = List("CTH" -> Location("1.2", "2.3", "CHDWLHT", "Chadwell Heath", "CTH", "XR", Some("Station"))) toMap
    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)

    val locations = enrichedLocations.locations
    locations.size should be(1)

    locations("CTH").location.lat should be(1.2)
    locations("CTH").location.lon should be(2.3)
    locations("CTH").tiploc should be(Set("CHDWLHT"))
    locations("CTH").name should be("Chadwell Heath")
    locations("CTH").crs should be(Set("CTH"))
    locations("CTH").id should be("CTH")
    locations("CTH").operator should be("XR")
    locations("CTH").`type` should be("Station")
  }

  it should "read in TIPLOC location with more data than location and fill it as one location" in {
    val tiplocMap: Map[String, RichTiploc] = List("CHDWLHT" -> RichTiploc("CHDWLHT" , "CTH", "Chadwell Heath", TiplocLocation("2.3", "3.4", "not-used", "not-used", "TOC", Some("Type")))).toMap
    val stationsMap: Map[String, Location] = List("CTH" -> Location("0.0", "0.0", "CHDWLHT", "Chadwell Heath", "CTH", "", None)) toMap
    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)

    val locations = enrichedLocations.locations
    locations.size should be(1)

    locations("CTH").location.lat should be(2.3)
    locations("CTH").location.lon should be(3.4)
    locations("CTH").tiploc should be(Set("CHDWLHT"))
    locations("CTH").name should be("Chadwell Heath")
    locations("CTH").crs should be(Set("CTH"))
    locations("CTH").id should be("CTH")
    locations("CTH").operator should be("TOC")
    locations("CTH").`type` should be("Station")
  }

  it should "Add subsidiary tiplocs for same crs code as one location" in {
    val tiplocMap: Map[String, RichTiploc] =
      List("RTFD" ->  RichTiploc("RTFD", "RET", "Retford", TiplocLocation("53.315092", "-0.94812569", "", "", "GR", None)),
           ("RTFDLL" -> RichTiploc("RTFDLL", "RET", "Retford", TiplocLocation("53.313874", "-0.94462822", "", "", "GR", None)))).toMap

    val stationsMap: Map[String, Location] = List("RET" -> Location("53.315092", "-0.94812569", "RTFD", "Retford", "RET", "GR", Some("Station"))) toMap
    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)

    val locations = enrichedLocations.locations
    locations.size should be(1)

    locations("RET").location.lat should be(53.315092)
    locations("RET").location.lon should be(-0.94812569)
    locations("RET").name should be("Retford")
    locations("RET").tiploc should be(Set("RTFD", "RTFDLL"))
    locations("RET").id should be("RET")
    locations("RET").crs should be(Set("RET"))
    locations("RET").operator should be("GR")
    locations("RET").`type` should be("Station")
  }

  it should "Add tiplocs for points as a separate location" in {
    val tiplocMap: Map[String, RichTiploc] =
    List("CHDWHLJ" -> RichTiploc("CHDWHLJ","CHDWHLJ","Chadwell Heath Junction",TiplocLocation("51.567929","0.12797209","547582","187596","XX",Some("Junction"))),
          "RTFDLL" -> RichTiploc("RTFDLL", "RET", "Retford", TiplocLocation("53.313874", "-0.94462822", "", "", "GR", None))).toMap


    val stationsMap: Map[String, Location] = List("RET" -> Location("53.315092", "-0.94812569", "RTFDLL", "Retford", "RET", "GR", Some("Station"))) toMap
    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)

    val locations = enrichedLocations.locations
    locations.size should be(2)

    locations("RET").location.lat should be(53.315092)
    locations("RET").location.lon should be(-0.94812569)
    locations("RET").name should be("Retford")
    locations("RET").tiploc should be(Set("RTFDLL"))
    locations("RET").id should be("RET")
    locations("RET").crs should be(Set("RET"))
    locations("RET").operator should be("GR")
    locations("RET").`type` should be("Station")

    locations("CHDWHLJ").location.lat should be(51.567929)
    locations("CHDWHLJ").location.lon should be(0.12797209)
    locations("CHDWHLJ").name should be("Chadwell Heath Junction")
    locations("CHDWHLJ").tiploc should be(Set("CHDWHLJ"))
    locations("CHDWHLJ").id should be("CHDWHLJ")
    locations("CHDWHLJ").crs should be(Set("CHDWHLJ"))
    locations("CHDWHLJ").operator should be("XX")
    locations("CHDWHLJ").`type` should be("Junction")
  }

  it should "populate change time information if location already exists with tiploc" in {
    val tiplocMap: Map[String, RichTiploc] = List("CHDWLHT" -> RichTiploc("CHDWLHT" , "CTH", "Chadwell Heath", TiplocLocation("2.3", "3.4", "not-used", "not-used", "TOC", Some("Type")))).toMap
    val stationsMap: Map[String, Location] = List("CTH" -> Location("0.0", "0.0", "CHDWLHT", "Chadwell Heath", "CTH", "", None)) toMap

    val nrMap: List[TTISFLocation] = List(TTISFLocation("CHADWELL HEATH","1","CHDWLHT","CTH","CTH","547650.0",false,"187610.0","5",Some("51.568036853828794"), Some("0.12895836922664597")))

    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)
      .withNationalRailLocationInfo(nrMap)

    val locations = enrichedLocations.locations
    locations.size should be(1)

    locations("CTH").nrInfo.get.interchangeType should be("1")
    locations("CTH").nrInfo.get.changeTime should be("5")
  }

  it should "populate subsiduary crs information for location that alrady exists with tiploc" in {
    val tiplocMap: Map[String, RichTiploc] = List("CHDWLHT" -> RichTiploc("CHDWLHT" , "CTH", "Chadwell Heath", TiplocLocation("2.3", "3.4", "not-used", "not-used", "TOC", Some("Type")))).toMap
    val stationsMap: Map[String, Location] = List("CTH" -> Location("0.0", "0.0", "CHDWLHT", "Chadwell Heath", "CTH", "", None)) toMap

    val nrMap: List[TTISFLocation] = List(TTISFLocation("CHADWELL HEATH","1","CHDWLHT","CTA","CTH","547650.0",false,"187610.0","5",Some("51.568036853828794"), Some("0.12895836922664597")))

    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)
      .withNationalRailLocationInfo(nrMap)

    val locations = enrichedLocations.locations
    locations.size should be(1)

    locations("CTH").crs should be(Set("CTH", "CTA"))
  }

  it should "Add locations from national rail locations that do not exist either the tiploc or locations file" in {
    val nrMap: List[TTISFLocation] = List(TTISFLocation("SCRABSTER","1","SCRBSTR","SCB","SCB","0.0",false,"0.0","10",Some("58.609722"), Some("-3.5525")))

    val tiplocMap: Map[String, RichTiploc] =
      List("CHDWHLJ" -> RichTiploc("CHDWHLJ","CHDWHLJ","Chadwell Heath Junction",TiplocLocation("51.567929","0.12797209","547582","187596","XX",Some("Junction"))),
        "RTFDLL" -> RichTiploc("RTFDLL", "RET", "Retford", TiplocLocation("53.313874", "-0.94462822", "", "", "GR", None))).toMap


    val stationsMap: Map[String, Location] = List("RET" -> Location("53.315092", "-0.94812569", "RTFDLL", "Retford", "RET", "GR", Some("Station"))) toMap
    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)
      .withNationalRailLocationInfo(nrMap)

    val locations = enrichedLocations.locations
    locations.size should be(3)
    locations("SCB").name should be("SCRABSTER")
    locations("SCB").`type` should be("Point")
    locations("SCB").id should be("SCB")
    locations("SCB").crs should be(Set("SCB"))
    locations("SCB").tiploc should be(Set("SCRBSTR"))
    locations("SCB").orrStation should be(false)
    locations("SCB").nrInfo.get.changeTime should be("10")
    locations("SCB").nrInfo.get.interchangeType should be("1")
    locations("SCB").location.lat should be(58.609722)
    locations("SCB").location.lon should be(-3.5525)
  }

  it should "Add update locations from national rail data if it doesnt already have it from tiploc" in {
    val nrMap: List[TTISFLocation] = List(TTISFLocation("SCRABSTER","1","SCRBSTR","SCB","SCB","0.0",false,"0.0","10",Some("58.609722"), Some("-3.5525")))

    val tiplocMap: Map[String, RichTiploc] = Map.empty

    val stationsMap: Map[String, Location] = List("SCB" -> Location("0.0", "0.0", "SCRBSTR", "Scrabster", "SCB", "XX", Some("BUS"))) toMap
    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)
      .withNationalRailLocationInfo(nrMap)

    val locations = enrichedLocations.locations
    locations.size should be(1)
    locations("SCB").name should be("Scrabster")
    locations("SCB").`type` should be("BUS")
    locations("SCB").id should be("SCB")
    locations("SCB").crs should be(Set("SCB"))
    locations("SCB").tiploc should be(Set("SCRBSTR"))
    locations("SCB").orrStation should be(false)
    locations("SCB").nrInfo.get.changeTime should be("10")
    locations("SCB").nrInfo.get.interchangeType should be("1")
    locations("SCB").location.lat should be(58.609722)
    locations("SCB").location.lon should be(-3.5525)
  }
  it should "not mark two stations as the same" in {
    val nrMap: List[TTISFLocation] = List(
      TTISFLocation("SHORTLANDS","2","SHRTLND","SRT","SRT","0.0",false,"0.0","4",Some("51.40579885288157"),Some("0.0017840279047206154")),
      TTISFLocation("SHREWSBURY","2","SHRWBY","SHR","SHR","0.0",false,"0.0","5",Some("52.71192676795479"),Some("-2.749761144951977"))
    )
    val tiplocMap: Map[String, RichTiploc] = List(
      "SHR" -> RichTiploc("SHRWBY","SHR","Shrewsbury",  TiplocLocation("52.710661","-2.7479928",  "0","0", "AW", Some("Station"))),
      "SRT" -> RichTiploc("SHRTLND","SRT","Shortlands", TiplocLocation("51.405625","0.0024809791","0","0", "SE", Some("Station")))).toMap


    val stationsMap: Map[String, Location] = List(
      "SRT" -> Location("51.40579885288157", "0.0017840279047206154", "SHRTLND", "Shortlands","SRT","SE", Some("Station")),
    "SHR" -> Location("52.71192676795479", " -2.749761144951977", "SHRWBY", "Shrewsbury", "SHR", "AW", Some("Station"))) toMap


    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)
      .withNationalRailLocationInfo(nrMap)

    val locations = enrichedLocations.locations

    locations.size should be(2)
  }

  it should "set station status based on ORR List of stations" in {
    val nrMap: List[TTISFLocation] = List.empty

    val tiplocMap: Map[String, RichTiploc] =
      List("CHDWHLJ" -> RichTiploc("CHDWHLJ","CHDWHLJ","Chadwell Heath Junction",TiplocLocation("51.567929","0.12797209","547582","187596","XX",Some("Junction")))).toMap


    val stations = List("RET")

    val stationsMap: Map[String, Location] = List("RET" -> Location("53.315092", "-0.94812569", "RTFDLL", "Retford", "RET", "GR", Some("Station"))) toMap
    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)
      .withNationalRailLocationInfo(nrMap)
      .withORRStationDefinitions(stations)

    val locations = enrichedLocations.locations
    locations.size should be(2)
    locations("RET").orrStation should be(true)
    locations("CHDWHLJ").orrStation should be(false)
  }

  it should "populate nr route information from ORR data" in {

    val nrMap: List[TTISFLocation] = List.empty
    val tiplocMap: Map[String, RichTiploc] = Map.empty
    val stations = List()

    val stations17Enhanced: Map[String, Estimate17StationEntry] =
      List("RET" -> Estimate17StationEntry("RET", "Retford", "Region", "Authority", "Constituency", 0.0, 0.0, "TOC", "A.01", "Anglia", "NR Route", "CRP Line", 1000)).toMap

    val stationsMap: Map[String, Location] = List("RET" -> Location("53.315092", "-0.94812569", "RTFDLL", "Retford", "RET", "GR", Some("Station"))) toMap
    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)
      .withNationalRailLocationInfo(nrMap)
      .withORRStationDefinitions(stations)
      .withORR2017Data(stations17Enhanced)

    val locations = enrichedLocations.locations
    locations.size should be(1)
    locations("RET").nrInfo.get.route should be("NR Route")
    locations("RET").nrInfo.get.crp should be("CRP Line")
    locations("RET").nrInfo.get.srs should be("A.01")
  }

  it should "populate location information from ORR data" in {

    val stations17Enhanced: Map[String, Estimate17StationEntry] = Map.empty
    val nrMap: List[TTISFLocation] = List.empty
    val tiplocMap: Map[String, RichTiploc] = Map.empty
    val stations = List()

    val stations11Enhanced: Map[String, Estimate11StationEntry] = List(
      "RET" -> Estimate11StationEntry("RET", "Retford", "location", "region", "county", "district")).toMap


    val stationsMap: Map[String, Location] = List("RET" -> Location("53.315092", "-0.94812569", "RTFDLL", "Retford", "RET", "GR", Some("Station"))) toMap
    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)
      .withNationalRailLocationInfo(nrMap)
      .withORRStationDefinitions(stations)
      .withORR2017Data(stations17Enhanced)
      .withOrr2011Data(stations11Enhanced)

    val locations = enrichedLocations.locations
    locations.size should be(1)
    locations("RET").location.county should be(Some("county"))
    locations("RET").location.postcode should be(Some("location"))
    locations("RET").location.district should be(Some("district"))
  }

  it should "read in all subsidiary tiplocs" in {
    val nrMap: List[TTISFLocation] = List(
      TTISFLocation("BECKENHAM JUNCTION","2","BCKNMJC","BKJ","BKJ","537380.0",false,"169865.0","4",Some("51.41103322280916"),Some("-0.025812819497362458")),
      TTISFLocation("BECKENHAM JUNCTION","2","BCKNHMJ","BKJ","BKJ","537380.0",false,"169865.0","4",Some("51.41103322280916"), Some("-0.025812819497362458")),
      TTISFLocation("BECKENHAM JUNCTION","9","BCKNBUS","BKJ","BKJ","537380.0",false,"169865.0","4",Some("51.41103322280916"),Some("-0.025812819497362458")))

    val tiplocMap: Map[String, RichTiploc] = Map(
      "BCKNHMJ"->RichTiploc("BCKNHMJ","BKJ","Beckenham Junction",TiplocLocation("51.411303","-0.026894082","537319","169878","SE", Some("Point"))),
      "BCKNMJC" -> RichTiploc("BCKNMJC","BKJ","Beckenham Junction",TiplocLocation("51.411303","-0.026894082","537319","169878","SE", Some("Point"))))


    val stationsMap: Map[String, Location] = Map(
      "BCKNHMJ" -> Location("51.41103322280916","-0.025812819497362458","BCKNHMJ","Beckenham Junction","BKJ","SE",Some("Point")),
      "BCKNBUS" -> Location("51.41103322280916","-0.025812819497362458","BCKNBUS","Beckenham Junction","BKJ","SE",Some("Point")),
      "BCKNMJC" -> Location("51.41103322280916","-0.025812819497362458","BCKNMJC","Beckenham Junction","BKJ","SE",Some("Point"))
      )

    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)
      .withNationalRailLocationInfo(nrMap)

    val locations = enrichedLocations.locations
    locations.size should be(1)
    locations("BKJ").tiploc.size should be(3)
    locations("BKJ").nrInfo.get.interchangeType should be("2")
    locations("BKJ").nrInfo.get.changeTime should be("4")
  }

  it should "read in all subsidiary tiplocs when not all in stations file" in {
    val nrMap: List[TTISFLocation] = List(
      TTISFLocation("BECKENHAM JUNCTION","9","BCKNMJC","BKJ","BKJ","537380.0",false,"169865.0","4",Some("51.41103322280916"),Some("-0.025812819497362458")))

    val tiplocMap: Map[String, RichTiploc] = Map(
      "BCKNHMJ"->RichTiploc("BCKNHMJ","BKJ","Beckenham Junction",TiplocLocation("51.411303","-0.026894082","537319","169878","SE", Some("Point"))),
      "BCKNMJC" -> RichTiploc("BCKNMJC","BKJ","Beckenham Junction",TiplocLocation("51.411303","-0.026894082","537319","169878","SE", Some("Point"))),
      "BCKNBUS" -> RichTiploc("BCKNBUS","BKJ","Beckenham Junction",TiplocLocation("51.411303","-0.026894082","537319","169878","SE", Some("Point"))))


    val stationsMap: Map[String, Location] = Map(
      "BCKNMJC" -> Location("51.41103322280916","-0.025812819497362458","BCKNMJC","Beckenham Junction","BKJ","SE",Some("Point"))
      )

    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)
      .withNationalRailLocationInfo(nrMap)

    val locations = enrichedLocations.locations
    locations.size should be(1)
    locations("BKJ").tiploc.size should be(3)
  }

  it should "read in all subsidiary tiplocs when in nr file" in {
    val nrMap: List[TTISFLocation] = List(
      TTISFLocation("BECKENHAM JUNCTION","9","BCKNMJC","BKJ","BKJ","537380.0",false,"169865.0","4",Some("51.41103322280916"),Some("-0.025812819497362458")),
      TTISFLocation("BECKENHAM JUNCTION","2","BCKNHMJ","BKJ","BKJ","537380.0",false,"169865.0","4",Some("51.41103322280916"), Some("-0.025812819497362458")),
      TTISFLocation("BECKENHAM JUNCTION","9","BCKNBUS","BKJ","BKJ","537380.0",false,"169865.0","4",Some("51.41103322280916"),Some("-0.025812819497362458")))

    val tiplocMap: Map[String, RichTiploc] = Map(
      "BCKNMJC" -> RichTiploc("BCKNMJC","BKJ","Beckenham Junction",TiplocLocation("51.411303","-0.026894082","537319","169878","SE", Some("Point"))))


    val stationsMap: Map[String, Location] = Map(
      "BCKNHMJ" -> Location("51.41103322280916","-0.025812819497362458","BCKNHMJ","Beckenham Junction","BKJ","SE",Some("Point"))
    )

    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(tiplocMap)
      .withNationalRailLocationInfo(nrMap)

    val locations = enrichedLocations.locations
    locations.size should be(1)
    locations("BKJ").tiploc.size should be(3)
  }

  it should "read in additional locations files" in {
    val additionalLocations: List[AdditionalLocation] = List(AdditionalLocation("Arlesford", "Heritage", "Watercress", "51.23", "-1.16", "", "New Arlesford", "Heritage", "Operator"))
    val stationsMap: Map[String, Location] = Map.empty
    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(Map.empty)
      .withNationalRailLocationInfo(List.empty)
      .withORRStationDefinitions(List.empty)
      .withORR2017Data(Map.empty)
      .withOrr2011Data(Map.empty)
      .withAdditionalLocations(additionalLocations)

    val loc: EnrichedLocation = enrichedLocations.locations("Arlesford")
    loc.id should be("HWARL")
    loc.name should be("Arlesford")
    loc.operator should be("Operator")
    loc.orrStation should be(false)
    loc.tiploc should be(Set.empty[String])
    loc.crs should be(Set.empty[String])
    loc.nrInfo should be(None)
    loc.nrInfo should be(None)
    loc.`type` should be("Heritage")
    loc.operator should be("Operator")
    loc.location.district should be(Some("New Arlesford"))
    loc.location.lat should be(51.23)
    loc.location.lon should be(-1.16)
  }

  it should "read in additional locations files and enhance already included stations" in {
    val additionalLocations: List[AdditionalLocation] = List(
      AdditionalLocation("Somewhere", "System", "Line 1", "51.23", "-1.16", "ABC", "Greater Somewhere", "Heritage", "Operator"),
      AdditionalLocation("Somewhere Else", "Another System", "Line 2", "12.23", "23.34", "", "Somewhere Else", "Heritage", "Operator")
    )
    val stationsMap: Map[String, Location] = Map("ABC" -> Location("51.23", "-1.16", "", "Somewhere", "ABC", "", Some("Heritage")))
    val enrichedLocations = StationsV2
      .convertLocationToEnrichedLocation(stationsMap)
      .withTiplocInformation(Map.empty)
      .withNationalRailLocationInfo(List.empty)
      .withORRStationDefinitions(List.empty)
      .withORR2017Data(Map.empty)
      .withOrr2011Data(Map.empty)
      .withAdditionalLocations(additionalLocations)

    enrichedLocations.locations.size should be(2)
    enrichedLocations.locations("ABC").operator should be("Operator")
    enrichedLocations.locations("ABC").location.district should be(Some("Greater Somewhere"))
    enrichedLocations.locations("ASL2SOE").name should be ("Somewhere Else")
  }

}
