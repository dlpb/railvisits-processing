package stations


import model._
import model.Location._

object StationsV2 {
  def convertLocationToEnrichedLocation(stationsMap: Map[String, Location]): StationsV2EnrichedJsonStations = {
    StationsV2EnrichedJsonStations(stationsMap.keySet.map {
      key =>
        val loc = stationsMap(key)
        val matchingLocs:List[Location] = stationsMap.values.filter(p => p.crs.equals(loc.crs)).toList

        loc.crs ->
          EnrichedLocation(
            loc.crs,
            loc.name,
            loc.toc,
            loc.`type`.getOrElse("Station"),
            SpatialLocation(loc.lat.toDouble, loc.lon.toDouble, None, None, None),
            None,
            false,
            matchingLocs.map {_.crs}.filter {_.length == 3}.toSet,
            matchingLocs.map {_.tiploc}.toSet)

    }.toMap)
  }


}

case class StationsV2EnrichedJsonStations(locations: Map[String, EnrichedLocation]) {

  def withTiplocInformation(tiplocMap: Map[String, RichTiploc]): StationsV2WithTiplocs = {
    val enriched: Map[String, EnrichedLocation] = locations.keySet.map {
      crs =>
        val loc: EnrichedLocation = locations(crs)
        val tiploc = tiplocMap.get(loc.tiploc.head)

        val additionalTiplocs: Set[String] = tiplocMap.values.filter(p => p.crs.equals(crs)) map {e => e.tiploc} toSet

        if(tiploc.isDefined){
          val t = tiploc.get
          val newLat = if(loc.location.lat.equals(0.0)) t.location.lat.toDouble else loc.location.lat
          val newLon = if(loc.location.lon.equals(0.0)) t.location.lon.toDouble else loc.location.lon
          val newType = if(!loc.`type`.equals("Station") && t.location.`type`.isDefined) t.location.`type`.get else loc.`type`
          val newToc = if(loc.operator.equals("RT") || loc.operator.equals("")) t.location.toc else loc.operator
          loc.id ->
            EnrichedLocation(
              loc.id,
              loc.name,
              newToc,
              newType,
              SpatialLocation(newLat, newLon,loc.location.county, loc.location.district, loc.location.postcode),
              loc.nrInfo,
              loc.orrStation,
              loc.crs,
              loc.tiploc ++ additionalTiplocs)
        }
        else
          loc.id -> loc
    }.toMap

    val unprocessedTiplocs = tiplocMap.filter(p => !enriched.keySet.contains(p._2.crs))
    val enrichedUnprocessedTiplocs: Map[String, EnrichedLocation] = unprocessedTiplocs map {
      t =>
        val (crs, tiploc) = t
        tiploc.crs ->
          EnrichedLocation(
            tiploc.crs,
            tiploc.name,
            tiploc.location.toc,
            tiploc.location.`type`.getOrElse("Infrastructure"),
            SpatialLocation(tiploc.location.lat.toDouble,
              tiploc.location.lon.toDouble, None, None, None),
            None,
            false,
            Set(tiploc.crs),
            Set(tiploc.tiploc))
    }
   StationsV2WithTiplocs(enriched ++ enrichedUnprocessedTiplocs)
  }
}

case class StationsV2WithTiplocs(locations: Map[String, EnrichedLocation]) {
  def withNationalRailLocationInfo(nrList: List[TTISFLocation]) : StationsV2WithNrLocations = {
    val enrichedLocations: Map[String, EnrichedLocation] = locations.keySet.map {
      id =>
        val loc = locations(id)
        val matchingNrLocations = nrList.filter(
          p => {
            id.equals(p.crs) ||
            id.equals(p.subsidiaryCrs)
        })

        val additionalCrs = (loc.crs ++ matchingNrLocations.map{_.subsidiaryCrs}.toSet) filter {_.length == 3}
        val additionalTiplocs = loc.tiploc ++ matchingNrLocations.map{_.tiploc.trim}.toSet

        val head = matchingNrLocations.headOption
        val nrInfo: Option[NrInfo] = makeNewNrInfo(Some(loc), head)

        val lat = if(head.isDefined && loc.location.lat.equals(0.0)) head.get.lat.get.toDouble else loc.location.lat
        val lon = if(head.isDefined && loc.location.lon.equals(0.0)) head.get.lon.get.toDouble else loc.location.lon

        loc.id ->
          EnrichedLocation(
            loc.id,
            loc.name,
            loc.operator,
            loc.`type`,
            SpatialLocation(lat, lon, loc.location.county, loc.location.district, loc.location.postcode),
            nrInfo,
            loc.orrStation,
            additionalCrs,
            additionalTiplocs)
    }.toMap

    val unprocessedNrLocations = nrList.filterNot(p => enrichedLocations.keySet.contains(p.crs))
    val enrichedNrLocations: Map[String, EnrichedLocation] = unprocessedNrLocations map {
      loc =>
        loc.crs ->
        EnrichedLocation(
          loc.crs,
          loc.name,
          "RT",
          "Point",
          SpatialLocation(loc.lat.get.toDouble, loc.lon.get.toDouble, None, None, None),
          makeNewNrInfo(None, Some(loc)),
          false,
          Set(loc.crs, loc.subsidiaryCrs).filter {_.length == 3},
          Set(loc.tiploc))
    } toMap

    StationsV2WithNrLocations(enrichedLocations ++ enrichedNrLocations)
  }

  private def makeNewNrInfo(loc: Option[EnrichedLocation], ttisfLocation: Option[TTISFLocation]): Option[NrInfo] = {
    val interchangeType: Option[String] =
      if (ttisfLocation.isDefined) Some(ttisfLocation.get.categoryType)
      else if (loc.isDefined && loc.get.nrInfo.isDefined) {
        val i = loc.get.nrInfo.get.interchangeType
        if (i.isEmpty)
          if (ttisfLocation.isDefined) Some(ttisfLocation.get.categoryType)
          else None
        else
          Some(i)
      }
      else if (ttisfLocation.isDefined) {
        Some(ttisfLocation.get.changeTime)
      }
      else None
    val changeTime: Option[String] =
      if (ttisfLocation.isDefined) Some(ttisfLocation.get.changeTime)
      else if (loc.isDefined && loc.get.nrInfo.isDefined) {
        val c = loc.get.nrInfo.get.changeTime
        if (c.isEmpty)
          if (ttisfLocation.isDefined) Some(ttisfLocation.get.changeTime)
          else None
        else
          Some(c)
      }
      else if (ttisfLocation.isDefined) {
        Some(ttisfLocation.get.categoryType)
      }
      else None

    val nrInfo: Option[NrInfo] = {
      val prevNrInfo = if(loc.isDefined) loc.get.nrInfo else None
      if (prevNrInfo.isDefined) {
        var nri = prevNrInfo.get
        if (interchangeType.isDefined) nri = NrInfo(nri.crp, nri.route, nri.srs, nri.changeTime, interchangeType.get)
        if (changeTime.isDefined) nri = NrInfo(nri.crp, nri.route, nri.srs, changeTime.get, nri.interchangeType)
        Some(nri)
      }
      else if (prevNrInfo.isEmpty && (changeTime.isDefined || interchangeType.isDefined)) {
        var nri = NrInfo("", "", "", "", "")
        if (interchangeType.isDefined) nri = NrInfo(nri.crp, nri.route, nri.srs, nri.changeTime, interchangeType.get)
        if (changeTime.isDefined) nri = NrInfo(nri.crp, nri.route, nri.srs, changeTime.get, nri.interchangeType)
        Some(nri)
      }
      else None

    }
    nrInfo
  }
}

case class StationsV2WithNrLocations(locations: Map[String, EnrichedLocation]) {
  def withORRStationDefinitions(stations: List[String]): StationsV2WithOrrDefinition = {
    val enrichedLocations = locations.keySet.map {
      crs =>
        val loc = locations(crs)
        val isStation = stations.contains(crs)
        val `type` = if(isStation) "Station" else loc.`type`
        loc.id ->
          EnrichedLocation(
            loc.id,
            loc.name,
            loc.operator,
            `type`,
            loc.location,
            loc.nrInfo,
            isStation,
            loc.crs,
            loc.tiploc)
    }.toMap
    StationsV2WithOrrDefinition(enrichedLocations)
  }
}

case class StationsV2WithOrrDefinition(locations: Map[String, EnrichedLocation]) {
  def withORR2017Data(orrLocs: Map[String, Estimate17StationEntry]) : StationsV2With2017OrrData = {
    val enrichedLocations = locations.keySet.map {
      crs =>
        val loc = locations(crs)
        val orrLoc = orrLocs.get(crs)

        val srs = if(orrLoc.isDefined) orrLoc.get.srsCode else if(loc.nrInfo.isDefined) loc.nrInfo.get.srs else ""
        val crp = if(orrLoc.isDefined) orrLoc.get.crpLine else if(loc.nrInfo.isDefined) loc.nrInfo.get.crp else ""
        val route = if(orrLoc.isDefined) orrLoc.get.nrRoute else if(loc.nrInfo.isDefined) loc.nrInfo.get.route else ""
        val changeTime = if(loc.nrInfo.isDefined) loc.nrInfo.get.changeTime else ""
        val interchangeType = if(loc.nrInfo.isDefined) loc.nrInfo.get.interchangeType else ""

        loc.id ->
          EnrichedLocation(
            loc.id,
            loc.name,
            loc.operator,
            loc.`type`,
            loc.location,
            Some(NrInfo(crp, route, srs, changeTime, interchangeType)),
            loc.orrStation,
            loc.crs,
            loc.tiploc)
    }.toMap
    StationsV2With2017OrrData(enrichedLocations)
  }

}

case class StationsV2With2017OrrData(locations: Map[String, EnrichedLocation]) {
  def withOrr2011Data(orrLocations: Map[String, Estimate11StationEntry]): StationsV2With2011Data = {
    val enrichedLocations = locations.keySet.map {
      crs =>
        val loc = locations(crs)
        val orrLoc = orrLocations.get(crs)

        val postcode: Option[String] = if(orrLoc.isDefined) Some(orrLoc.get.location) else loc.location.postcode
        val district: Option[String] = if(orrLoc.isDefined) Some(orrLoc.get.district) else loc.location.district
        val county: Option[String]   = if(orrLoc.isDefined) Some(orrLoc.get.county) else loc.location.county

        crs ->
          EnrichedLocation(
            loc.id,
            loc.name,
            loc.operator,
            loc.`type`,
            SpatialLocation(loc.location.lat, loc.location.lon, county, district, postcode),
            loc.nrInfo,
            loc.orrStation,
            loc.crs,
            loc.tiploc)
    }.toMap
    StationsV2With2011Data(enrichedLocations)
  }

}

case class StationsV2With2011Data(locations: Map[String, EnrichedLocation]){


  def withAdditionalLocations(additionalLocations: List[AdditionalLocation]): StationsV2With2011Data = {

    def getFirstCharacterOfEachWordIn(name: String): String = {
      name.trim.split(" ").toList map (_.head) mkString
    }

    def getStationKey(station: String): String = {
      val noIllegalChars = station.replaceAll("[^a-zA-Z0-9 ]", "").replaceAll(" +", " ")
      val stationWords = noIllegalChars.split(" ").toList
      stationWords.size match {
        case 1 => station.substring(0,3)
        case 2 => stationWords.head.substring(0,2) + stationWords.tail.head.substring(0,1)
        case _ => getFirstCharacterOfEachWordIn(noIllegalChars)
      }
    }

    var generatedIds: Set[String] = Set.empty

    def getId(name: String, system: String, line: String): String = {
      val systemKey = getFirstCharacterOfEachWordIn(system)
      val lineKey = getFirstCharacterOfEachWordIn(line)
      val stationKey = getStationKey(name)
      val id = systemKey + lineKey + stationKey toUpperCase()
      if(generatedIds.contains(id)){
        val nameDroppedChar = name.charAt(0) + name.substring(2)
        getId(nameDroppedChar,system,line)
      }
      else {
        generatedIds = generatedIds + id
        id
      }
    }

    val locationsNotAlreadyIncluded = additionalLocations.filterNot(p => locations.keySet.contains(p.nrInterchange))
    val enrichedNotIncludedLocations: Map[String, EnrichedLocation] = locationsNotAlreadyIncluded.map {
      loc =>
        val id = getId(loc.name, loc.system, loc.line)
        id -> EnrichedLocation(
         id,
         loc.name,
         loc.operator,
         loc.`type`,
         SpatialLocation(loc.lat.toDouble, loc.lon.toDouble,None, Some(loc.location), None),
         None,
         false,
         Set.empty,
         Set.empty)
    }.toMap

    val enrichedIncludedLocations = additionalLocations.filter(p => locations.keySet.contains(p.nrInterchange)).map {
      loc =>
        val location = locations(loc.nrInterchange)

        val toc = if(location.operator.isEmpty) loc.operator else location.operator
        val district: Option[String] = if(location.location.district.isEmpty) Some(loc.location) else location.location.district
        location.id ->
          EnrichedLocation(
            location.id,
            location.name,
            toc,
            location.`type`,
            SpatialLocation(location.location.lat, location.location.lon, location.location.county, district, location.location.postcode),
            location.nrInfo,
            location.orrStation,
            location.crs,
            location.tiploc)
    }.toMap
    val locs = enrichedIncludedLocations ++ enrichedNotIncludedLocations
    val allLocs = locs ++ locations.filterNot(p => locs.keySet.contains(p._1))
    StationsV2With2011Data(allLocs)
  }
}