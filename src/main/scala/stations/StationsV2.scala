package stations


import model._
import model.Location._

object StationsV2 {
  def convertLocationToEnrichedLocation(stationsMap: Map[String, Location]): StationsV2EnrichedJsonStations = {
    StationsV2EnrichedJsonStations(stationsMap.keySet.map {
      key =>
        val loc = stationsMap(key)
        loc.crs ->
          EnrichedLocation(
            loc.name,
            loc.tiploc,
            loc.crs,
            loc.toc,
            loc.`type`.getOrElse("Station"),
            SpatialLocation(loc.lat.toDouble, loc.lon.toDouble, "", "", ""),
            NrInfo("", "", ""),
            false,
            "",
            "",
            Set()
          )

    }.toMap)
  }


}

case class StationsV2EnrichedJsonStations(locations: Map[String, EnrichedLocation]) {
  def withTiplocInformation(tiplocMap: Map[String, RichTiploc]): StationsV2WithTiplocs = {
    val enriched: Map[String, EnrichedLocation] = locations.keySet.map {
      crs =>
        val loc = locations(crs)
        val tiploc = tiplocMap.get(loc.tiploc)

        val additionalTiplocs = Set(loc.tiploc) ++ (tiplocMap.values.filter(p => p.crs.equals(crs)) map {e => e.tiploc} toSet)

        if(tiploc.isDefined){
          val t = tiploc.get
          val newLat = if(loc.location.lat.equals(0.0)) t.location.lat.toDouble else loc.location.lat
          val newLon = if(loc.location.lon.equals(0.0)) t.location.lon.toDouble else loc.location.lon
          val newType = if(!loc.`type`.equals("Station") && t.location.`type`.isDefined) t.location.`type`.get else loc.`type`
          val newToc = if(loc.toc.equals("XX") || loc.toc.equals("")) t.location.toc else loc.toc
          loc.crs ->
            EnrichedLocation(
              loc.name,
              loc.tiploc,
              loc.crs,
              newToc,
              newType,
              SpatialLocation(newLat, newLon,loc.location.county, loc.location.district, loc.location.postcode),
              loc.nrInfo,
              loc.station,
              loc.changeTime,
              loc.interchangeType,
              loc.subsidiaryCrs,
              loc.subsidiaryTiplocs ++ additionalTiplocs
            )
        }
        else
          loc.crs -> loc
    }.toMap

    val unprocessedTiplocs = tiplocMap.filter(p => !enriched.keySet.contains(p._2.crs))
    val enrichedUnprocessedTiplocs: Map[String, EnrichedLocation] = unprocessedTiplocs map {
      t =>
        val (crs, tiploc) = t
        tiploc.crs ->
          EnrichedLocation(
            tiploc.name,
            tiploc.tiploc,
            tiploc.crs,
            tiploc.location.toc,
            tiploc.location.`type`.getOrElse("Infrastructure"),
            SpatialLocation(tiploc.location.lat.toDouble, tiploc.location.lon.toDouble, "", "", ""),
            NrInfo("", "", ""),
            false,
            "",
            "",
            Set(tiploc.crs),
            Set(tiploc.tiploc)
          )
    }
   StationsV2WithTiplocs(enriched ++ enrichedUnprocessedTiplocs)
  }
}

case class StationsV2WithTiplocs(locations: Map[String, EnrichedLocation]) {
  def withNationalRailLocationInfo(nrList: List[TTISFLocation]) : StationsV2WithNrLocations = {
    val enrichedLocations = locations.keySet.map {
      crs =>
        val loc = locations(crs)
        val matchingNrLocations = nrList.filter(
          p => {
//            if(crs.equals("SRT")||crs.equals("SHR")){
//              println(s"CRS: $crs, p.crs:: ${p.crs}, p.sub: ${p.subsidiaryCrs}")
//            }
            crs.equals(p.crs) ||
            crs.equals(p.subsidiaryCrs)
//            loc.subsidiaryCrs.contains(p.crs) ||
//            loc.subsidiaryCrs.contains(p.subsidiaryCrs)
        })

        val additionalCrs = Set(crs) ++ loc.subsidiaryCrs ++ matchingNrLocations.map{_.subsidiaryCrs}.toSet

        val head = matchingNrLocations.headOption
        val interchangeType = if (head.isDefined) head.get.categoryType else loc.interchangeType
        val changeTime = if (head.isDefined) head.get.changeTime else loc.changeTime

        val lat = if(head.isDefined && loc.location.lat.equals(0.0)) head.get.lat.get.toDouble else loc.location.lat
        val lon = if(head.isDefined && loc.location.lon.equals(0.0)) head.get.lon.get.toDouble else loc.location.lon

        loc.crs ->
          EnrichedLocation(
            loc.name,
            loc.tiploc,
            loc.crs,
            loc.toc,
            loc.`type`,
            SpatialLocation(lat, lon, loc.location.county, loc.location.district, loc.location.postcode),
            loc.nrInfo,
            loc.station,
            changeTime,
            interchangeType,
            additionalCrs,
            loc.subsidiaryTiplocs
          )
    }.toMap

    val unprocessedNrLocations = nrList.filterNot(p => enrichedLocations.keySet.contains(p.crs))
    val enrichedNrLocations = unprocessedNrLocations map {
      loc =>
        loc.crs ->
        EnrichedLocation(
          loc.name,
          loc.tiploc,
          loc.crs,
          "XX",
          "Point",
          SpatialLocation(loc.lat.get.toDouble, loc.lon.get.toDouble, "", "", ""),
          NrInfo("", "", ""),
          false,
          loc.changeTime,
          loc.categoryType,
          Set(loc.crs, loc.subsidiaryCrs),
          Set(loc.tiploc)
        )
    }
    StationsV2WithNrLocations(enrichedLocations ++ enrichedNrLocations)
  }
}

case class StationsV2WithNrLocations(locations: Map[String, EnrichedLocation]) {
  def withORRStationDefinitions(stations: List[String]): StationsV2WithOrrDefinition = {
    val enrichedLocations = locations.keySet.map {
      crs =>
        val loc = locations(crs)
        val isStation = stations.contains(crs)
        loc.crs ->
          EnrichedLocation(
            loc.name,
            loc.tiploc,
            loc.crs,
            loc.toc,
            loc.`type`,
            loc.location,
            loc.nrInfo,
            isStation,
            loc.changeTime,
            loc.interchangeType,
            loc.subsidiaryCrs,
            loc.subsidiaryTiplocs
          )
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

        val srs = if(orrLoc.isDefined) orrLoc.get.srsCode else loc.nrInfo.srs
        val crp = if(orrLoc.isDefined) orrLoc.get.crpLine else loc.nrInfo.crp
        val route = if(orrLoc.isDefined) orrLoc.get.nrRoute else loc.nrInfo.route

        loc.crs ->
          EnrichedLocation(
            loc.name,
            loc.tiploc,
            loc.crs,
            loc.toc,
            loc.`type`,
            loc.location,
            NrInfo(crp, route, srs),
            loc.station,
            loc.changeTime,
            loc.interchangeType,
            loc.subsidiaryCrs,
            loc.subsidiaryTiplocs
          )
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

        val postcode = if(orrLoc.isDefined) orrLoc.get.location else loc.location.postcode
        val district = if(orrLoc.isDefined) orrLoc.get.district else loc.location.district
        val county = if(orrLoc.isDefined) orrLoc.get.county else loc.location.county

        crs ->
          EnrichedLocation(
            loc.name,
            loc.tiploc,
            loc.crs,
            loc.toc,
            loc.`type`,
            SpatialLocation(loc.location.lat, loc.location.lon, county, district, postcode),
            loc.nrInfo,
            loc.station,
            loc.changeTime,
            loc.interchangeType,
            loc.subsidiaryCrs,
            loc.subsidiaryTiplocs
          )
    }.toMap
    StationsV2With2011Data(enrichedLocations)
  }

}

case class StationsV2With2011Data(locations: Map[String, EnrichedLocation])