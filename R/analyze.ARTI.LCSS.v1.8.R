analyze.ARTI.LCSS.v1.8 <- function(){
  #Use this one for GPX exports!
  #Required packages for analysis.
  require(data.table)
  require(bit64)
  require(rgdal)
  require(geosphere)
  require(circular)
  require(sp)
  require(ggmap)
  #What time is it?
  start.time <- Sys.time()
  #Define some variables.
  working.directory<-file.path(path.expand("~"),"Match_Garmin_GPS_with_ARTI_Tracks")
  
  ####################MODIFY ME#####################  
  #Who Set the GPS wrong... by how many seconds?
  GPS.Offset.sec <- 0
  GPS.TimeZone<-"UTC"
  #Is the ARTI time off... by how many seconds?
  ARTI.Offset.sec <- 0
  #Position Allowance... radius in meters
  POS.Offset.m <- 26
  #Heading Allowance, degrees from North
  Head.Offset.deg <- 5
  #Speed Allowance, m/s
  Speed.Offset.m.s. <- 2
  #Map Type
  map.type<- "satellite"
  ####################MODIFY ME#####################
  
  #Read in the GPS data.
  setwd(file.path(working.directory,"CSV_to_Process","GPS"))
  gps.filelist<-list.files(pattern="\\.csv$")
  datalist<-lapply(gps.filelist,read.csv,header=T)
  GPS.data<-rbindlist(datalist)
  #Cleanup
  rm(datalist)
  setnames(GPS.data, c("Longitude","Latitude","Speed.mph","Speed.m.s.","Heading"),c("GPS.Lon","GPS.Lat","GPS.Speed.mph","GPS.Speed.m.s.","GPS.Heading"))
  #GPS.data$GPS.Speed.m.s.<-GPS.data$GPS.Speed.kmh * 0.277778
  #GPS.data$GPS.Speed.mph<-GPS.data$GPS.Speed.kmh * 0.621371
  
  #Make the time correct
  GPS.data$Update.Time<- as.POSIXct(GPS.data$Update.Time,format="%Y-%m-%d %H:%M:%S",tz=GPS.TimeZone)
  #Convert to GMT
  GPS.data$Update.Time.GMT<- as.character(GPS.data$Update.Time,tz="GMT")
  GPS.data$Update.Time<-as.POSIXct(GPS.data$Update.Time.GMT,tz = "GMT")
  #Zones as Factors  
  #GPS.data$ThermZone<-as.factor(GPS.data$ThermZone)
  #GPS.data$VisZone<-as.factor(GPS.data$VisZone)
  #Headings as Circular  
  GPS.data$GPS.Heading<-circular(GPS.data$GPS.Heading,type="angles",units="degrees",template="none",modulo="2pi",rotation="clock")
  
  assign("GPS.data",GPS.data,.GlobalEnv)
  
  #ARTI Data
  setwd(file.path(working.directory,"CSV_to_Process","ARTI"))
  arti.filelist<-list.files(pattern="\\.csv$")
  datalist<-lapply(arti.filelist,fread)
  #Make it one large dataset
  ARTI.data<-rbindlist(datalist)
  #Cleanup
  rm(datalist)
  arti.header<-unlist(read.csv(arti.filelist[1],header=F,nrows=1,stringsAsFactors = F))
  setnames(ARTI.data, c(arti.header),c("Update.Time", "Track.ID", "Start.Time..UTM.", "Latitude", "Longitude", "Speed..m.s.", "Heading..deg.N.", "Height...m.", "RCS..dBsm.", "Range.from.radar...m.", "Azimuth.from.radar..deg..", "Intensity", "UTM.Zone", "UTM.Northing", "UTM.Easting"))
  #Make the time correct
  ARTI.data$Update.Time <- as.POSIXct(ARTI.data$Update.Time, format="%Y/%m/%d %H:%M:%S", tz="GMT")
  #Adjust for any offset in time
  ARTI.data$Update.Time <- ARTI.data$Update.Time + ARTI.Offset.sec
  #Headings as Circular
  ARTI.data$Heading..deg.N.<-circular(as.numeric(ARTI.data$Heading..deg.N.),type="angles",units="degrees",template="none",modulo="2pi",rotation="clock")
  #Create a more unique ID
  ARTI.data$long.id<-paste0(ARTI.data$Start.Time..UTM.,ARTI.data$Track.ID)
  #Speed as numeric
  ARTI.data$Speed..m.s.<-as.numeric(ARTI.data$Speed..m.s.)
  #Long Lat as numeric
  ARTI.data$Longitude<-as.numeric(ARTI.data$Longitude)
  ARTI.data$Latitude<-as.numeric(ARTI.data$Latitude)
  #Range from radar as numeric
  ARTI.data$Range.from.radar...m.<-as.numeric(ARTI.data$Range.from.radar...m.)
  #Azimuth as numeric
  ARTI.data$Azimuth.from.radar..deg..<-as.numeric(ARTI.data$Azimuth.from.radar..deg..)
  
  assign("ARTI.data",ARTI.data,.GlobalEnv)
  
  ###### Now the fun part... let's match the Track data to the GPS Data #####
  ##Time
  sub.time<-merge(ARTI.data,GPS.data,by="Update.Time")
  ##Calculate Heading Difference
  sub.time$Heading.Offset<- abs(sub.time$Heading..deg.N. - sub.time$GPS.Heading) 
  sub.time$Heading.Offset<- ifelse(sub.time$Heading.Offset > 180, 360-sub.time$Heading.diff,sub.time$Heading.Offset)
  ##Position
  #Classify the coordinates as Spatial Data.
  GPS.longlatcoor<- SpatialPoints(cbind(sub.time$GPS.Lon,sub.time$GPS.Lat), proj4string=CRS("+proj=longlat +datum=WGS84"))
  arti.longlatcoor<- SpatialPoints(cbind(sub.time$Longitude,sub.time$Latitude), proj4string=CRS("+proj=longlat +datum=WGS84"))
  sub.time$Distance.Offset.m<- distGeo(arti.longlatcoor,GPS.longlatcoor)
  sub.pos<- subset(sub.time, Distance.Offset.m <= POS.Offset.m)
  #sub.pos<-subset(sub.time, UTM.Northing >= GPS.Northing - POS.Offset.m & UTM.Northing <= GPS.Northing + POS.Offset.m)
  #sub.pos<-subset(sub.pos, UTM.Easting >= GPS.Easting - POS.Offset.m & UTM.Easting <= GPS.Easting + POS.Offset.m)
  ##Heading
  sub.head<-subset(sub.pos, Heading.Offset <= Head.Offset.deg)
  ##Speed
  sub.speed<-subset(sub.head, Speed..m.s. >= GPS.Speed.m.s. - Speed.Offset.m.s. & Speed..m.s. <= GPS.Speed.m.s. + Speed.Offset.m.s.)
  assign("Matching.Track.Updates",sub.speed,.GlobalEnv)
  
  #Calculate accuracy of Accipiter System
  Matching.Track.Updates$Speed.Offset.m.s.<- Matching.Track.Updates$Speed..m.s. - Matching.Track.Updates$GPS.Speed.m.s.
  Matching.Track.Updates$Speed.Offset.mph<- Matching.Track.Updates$Speed.Offset.m.s. * 2.23694
  assign("Matching.Track.Updates",Matching.Track.Updates,.GlobalEnv)
  
  #Extract full tracks. Pull all the updates associated with the matches
  full.tracks<-subset(ARTI.data, long.id %in% unique(Matching.Track.Updates$long.id))
  assign("Full.Tracks",full.tracks,.GlobalEnv)
  
  
  #Map the results
  Matching.Track.Updates$long.id<- as.factor(Matching.Track.Updates$long.id) ##We'll need this to be a factor.
  Full.Tracks$long.id<- as.factor(Full.Tracks$long.id) ##We'll need this to be a the same factor as above.
  for(z in 1:length(unique(Matching.Track.Updates$long.id))){
    #Get the data for the map, 1 Matched Track at a time
    sub.Matching.Track.Updates<- subset(Matching.Track.Updates, as.integer(long.id)==z)
    sub.Full.Tracks<- subset(Full.Tracks, as.integer(long.id)==z)
    sub.GPS.data<- subset(GPS.data, Update.Time <= max(sub.Full.Tracks$Update.Time) & Update.Time >= min(sub.Full.Tracks$Update.Time))
    #Create the data.frame for plotting
    ##Matched
    plot.Matching.Track.Updates<- data.frame(sub.Matching.Track.Updates$Update.Time,sub.Matching.Track.Updates$Longitude,sub.Matching.Track.Updates$Latitude)
    plot.Matching.Track.Updates$Description<-"Matching Update"
    colnames(plot.Matching.Track.Updates)<-c("Update.Time","Longitude","Latitude","Description")
    ##Full Track
    plot.Full.Tracks<- data.frame(sub.Full.Tracks$Update.Time,sub.Full.Tracks$Longitude,sub.Full.Tracks$Latitude)
    plot.Full.Tracks$Description<- paste("Track",sub.Full.Tracks$Track.ID[1])
    colnames(plot.Full.Tracks)<-c("Update.Time","Longitude","Latitude","Description")
    ##GPS Data
    plot.GPS.data<- data.frame(sub.GPS.data$Update.Time,sub.GPS.data$GPS.Lon,sub.GPS.data$GPS.Lat)
    plot.GPS.data$Description<- "GPS Data"
    colnames(plot.GPS.data)<-c("Update.Time","Longitude","Latitude","Description")
    ##Final
    plot.final<-rbind(plot.Matching.Track.Updates,plot.Full.Tracks,plot.GPS.data)
    plot.final$hj<- ifelse(plot.final$Description == "GPS Data", 0,1)
    #Get the map, centered and zoomed on the Matched Track
    location<- make_bbox(Longitude, Latitude, plot.Full.Tracks, f = 8)
    #zoom<-ifelse(calc_zoom(location>19),17,calc_zoom(location))
    map<- get_map(location, zoom = 17, maptype = map.type)
    #map<-get_map(location = make_bbox(lon=Longitude,lat=Latitude,data=plot.Full.Tracks,f=8), source="google",maptype = map.type)
    #Get map attirbutes (corners)  
    bb<-attr(map,"bb")
    assign("bb",bb,.GlobalEnv)
    #Use map attributes to create a scale bar
    sbar <- data.frame(lon.start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
                       lon.end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
                       lat.start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                       lat.end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)))
    sbar$distance = distGeo(p1 = c(sbar$lon.start,sbar$lat.start),p2 = c(sbar$lon.end,sbar$lat.end)) #Distance across downloaded map in meters
    ptspermm <- 2.83464567  # need this because geom_text uses mm, and themes use pts. Urgh.
    #Plot it
    p<-ggmap(map,extent = "normal",maprange = F)
    m<-p + geom_point(data=plot.final,aes(x=Longitude,y=Latitude,color=Description,shape=Description),size=2.5) + scale_shape(solid = FALSE) + scale_colour_hue(l=40) + geom_text(data=plot.final,aes(x=Longitude, y=Latitude, label=Update.Time,hjust=hj),size=2,position="identity") + theme_nothing(legend = T,) + theme(legend.position = c(.85,.15),axis.text.x=element_blank(),axis.text.y=element_blank())
    final.graph<-m + geom_segment(data=sbar,aes(x=lon.start,xend=lon.end,y=lat.start,yend=lat.end))+geom_text(data = sbar, aes(x = (lon.start + lon.end)/2, y = lat.start + 0.025*(bb$ur.lat - bb$ll.lat),label = paste(format(distance*.000621371, digits = 4,nsmall = 2),'mi')),hjust = 0.5, vjust = 0, size = 8/ptspermm)  + coord_map(projection="mercator",xlim=c(bb$ll.lon, bb$ur.lon),ylim=c(bb$ll.lat, bb$ur.lat))+theme(plot.title = element_text(lineheight=.8, face="bold"))+theme(axis.text = element_text(size=8))#Add Scale Bar
    #Name it!
    map.filename<-paste0(as.character(sub.Full.Tracks$Update.Time[1],format="%Y.%m.%d_%H%M%S"),"_Matched_Track_",sub.Full.Tracks$Track.ID[1],".jpeg")
    #Save it!
    ggsave(file=file.path(working.directory,"Results","Maps",map.filename),plot=final.graph,width= 6,height=6,pointsize=12,units="in")
  }
  
  #Finish up
  #Move the ARTI Tracks to finished directory
  file.rename(from = file.path(working.directory,"CSV_to_Process","ARTI",arti.filelist),to = file.path(working.directory,"CSV_Finished","ARTI",arti.filelist))
  #Donezo
  end.time <- Sys.time()
  runtime.sec <- difftime(end.time, start.time, units=c("secs"))
  parameters<-data.frame(GPS.Offset.sec,ARTI.Offset.sec,POS.Offset.m,Head.Offset.deg,Speed.Offset.m.s.,runtime.sec)
  
  matched.filename<-paste0(as.character(ARTI.data$Update.Time[1],format="%Y_%b"),"_ARTI_Matched_",as.character(start.time,format="%Y.%m.%d_%H.%M.%S"),".csv")
  parameters.filename<-paste0(as.character(ARTI.data$Update.Time[1],format="%Y_%b"),"_ARTI_Matched_",as.character(start.time,format="%Y.%m.%d_%H.%M.%S"),"_Parameters.csv")
  full.tracks.filename<-paste0(as.character(ARTI.data$Update.Time[1],format="%Y_%b"),"_Full_Tracks_",as.character(start.time,format="%Y.%m.%d_%H.%M.%S"),".csv")
  write.csv(Matching.Track.Updates,file=file.path(working.directory,"Results",matched.filename),row.names = F)
  write.csv(parameters,file=file.path(working.directory,"Results",parameters.filename),row.names = F)
  write.csv(full.tracks,file=file.path(working.directory,"Results",full.tracks.filename),row.names = F)
}