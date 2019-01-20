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
    enrichedLocation("CTH").tiploc should be("CHDWLHT")
    enrichedLocation("CTH").name should be("Chadwell Heath")
    enrichedLocation("CTH").crs should be("CTH")
    enrichedLocation("CTH").toc should be("XR")
    enrichedLocation("CTH").`type` should be("Station")

    enrichedLocation("CTH").location.county should be("")
    enrichedLocation("CTH").location.district should be("")
    enrichedLocation("CTH").location.postcode should be("")
    enrichedLocation("CTH").nrInfo.srs should be("")
    enrichedLocation("CTH").nrInfo.crp should be("")
    enrichedLocation("CTH").nrInfo.route should be("")
    enrichedLocation("CTH").station should be(false)
    enrichedLocation("CTH").changeTime should be("")
    enrichedLocation("CTH").interchangeType should be("")
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
    locations("CTH").tiploc should be("CHDWLHT")
    locations("CTH").name should be("Chadwell Heath")
    locations("CTH").crs should be("CTH")
    locations("CTH").toc should be("XR")
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
    locations("CTH").tiploc should be("CHDWLHT")
    locations("CTH").name should be("Chadwell Heath")
    locations("CTH").crs should be("CTH")
    locations("CTH").toc should be("TOC")
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
    locations("RET").tiploc should be("RTFD")
    locations("RET").name should be("Retford")
    locations("RET").subsidiaryTiplocs should be(Set("RTFD", "RTFDLL"))
    locations("RET").crs should be("RET")
    locations("RET").toc should be("GR")
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
    locations("RET").tiploc should be("RTFDLL")
    locations("RET").name should be("Retford")
    locations("RET").subsidiaryTiplocs should be(Set("RTFDLL"))
    locations("RET").crs should be("RET")
    locations("RET").toc should be("GR")
    locations("RET").`type` should be("Station")

    locations("CHDWHLJ").location.lat should be(51.567929)
    locations("CHDWHLJ").location.lon should be(0.12797209)
    locations("CHDWHLJ").tiploc should be("CHDWHLJ")
    locations("CHDWHLJ").name should be("Chadwell Heath Junction")
    locations("CHDWHLJ").subsidiaryTiplocs should be(Set("CHDWHLJ"))
    locations("CHDWHLJ").crs should be("CHDWHLJ")
    locations("CHDWHLJ").toc should be("XX")
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

    locations("CTH").interchangeType should be("1")
    locations("CTH").changeTime should be("5")
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

    locations("CTH").subsidiaryCrs should be(Set("CTH", "CTA"))
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
    locations("SCB").crs should be("SCB")
    locations("SCB").tiploc should be("SCRBSTR")
    locations("SCB").station should be(false)
    locations("SCB").changeTime should be("10")
    locations("SCB").interchangeType should be("1")
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
    locations("SCB").crs should be("SCB")
    locations("SCB").tiploc should be("SCRBSTR")
    locations("SCB").station should be(false)
    locations("SCB").changeTime should be("10")
    locations("SCB").interchangeType should be("1")
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
    locations("RET").station should be(true)
    locations("CHDWHLJ").station should be(false)
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
    locations("RET").nrInfo.route should be("NR Route")
    locations("RET").nrInfo.crp should be("CRP Line")
    locations("RET").nrInfo.srs should be("A.01")
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
    locations("RET").location.county should be("county")
    locations("RET").location.postcode should be("location")
    locations("RET").location.district should be("district")
  }
}
