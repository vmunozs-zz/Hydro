#' Find Gauge Stations in Northamerica Environment Canada & USGS
#'
#' Find the closest stations n from the HYDAT & USGS database based on latitude and longitude
#' @param lat   Latitude in decimal degrees
#' @param long  Longitude in decimal degrees (in NorthAmerica is negative)
#' @param n     Amount of station to be reviewed
#' @export
NAf.find<-function(lat=62.895278, lon=-139.377426, n=100){
        #lat=63.921190; long=-135.506710; n=200
        #lat=58.204576; long=-103.711064; n=5
        library(RODBC) #Database Access
        library(HYDAT) #Hydat Access
        library(geosphere) #Distance from coordinates
        library(hydroTSM)
        library(lattice)
        library(waterData) #USGS data flow
        library(dataRetrieval)#USGS data flow

        # Windows only (default path to EC Data Explorer)
        hydatPath <- 'c:/program files (x86)/EC/ECdataexplorer/database/hydat.mdb'

        #read USGS Database
        #Gauge.Stations.USGS <- read.csv("Gauge Stations USGS.csv", stringsAsFactors=FALSE)
        data(station.db.USGS)

        dbc <- odbcConnectAccess( hydatPath )

        #Include the interesting point
        point.location<-c(Longitude=lon,Latitude=lat)

        # path to EC data explorer
        network <- ReadHydex(
                conn = dbc
        )

        # close DB connection
        odbcClose(dbc)

        #Removing regulated stations
        regulated<-is.na(network$regulated)
        network.unregulated<-network[regulated,]


        #Obtain coordiantes
        station.coordinates.ca<-cbind(network.unregulated$longitude,network.unregulated$latitude)

        #Obtain distance from interesting point to Gauge stations. Info is passed to Km and rounded.
        distance.km.ca<-round(distVincentySphere(point.location,station.coordinates.ca)/1000,2)

        #Including distance in the network file
        network.unregulated.ca<-cbind(network.unregulated,country="CA",distance.km=distance.km.ca)

        station.coordinates.us<-cbind(Gauge.Stations.USGS$Longitude,Gauge.Stations.USGS$Latitude)

        #Obtain distance from interesting point to Gauge stations. Info is passed to Km and rounded.
        distance.km.us<-round(distVincentySphere(point.location,station.coordinates.us)/1000,2)

        #Including distance in the network file
        network.us<-data.frame(id=Gauge.Stations.USGS$Station.ID,
                               name=Gauge.Stations.USGS$Station.Name,
                               provstate=NA,
                               latitude=Gauge.Stations.USGS$Latitude,
                               longitude=Gauge.Stations.USGS$Longitude,
                               areagross=NA,
                               areaeffective=NA,
                               hydrometricstatus=NA,
                               sedimentstatus=NA,
                               regulated=NA,
                               rhbn=NA,
                               realtime=NA,
                               country="US",
                               distance.km=distance.km.us,
                               stringsAsFactors = FALSE)

        network.na<-rbind(network.unregulated.ca,network.us)

        #Sorted unregulated stations
        network.sorted<-network.na[order(network.na$distance.km),]


        #First n stations
        NAflow.idx<-network.sorted
        selected.id<-NAflow.idx$id

        #Initial Values to be transfer
        stations.id<-list()
        flow.null<-list()

        # i is the counter for the station selected
        i<-0

        #station is the amount of stations selected
        stations<-0
        while(stations<n){

                i<-i+1
                if(NAflow.idx$country[i]=="CA"){
                        #i<-1
                        # open database connection
                        dbc <- odbcConnectAccess(hydatPath)
                        data <- ReadHydat ( conn = dbc, stationID = selected.id[i] )

                        # close database connection
                        odbcClose(dbc)

                        # display structure of returned data object

                        daily<-data$flow$daily  #Daily Information
                        #name<-data$metadata$name #Station Name
                        id<-data$metadata$id
                        #print(i)
                        if (is.null(daily)==TRUE){
                                flow.null[i]<-FALSE
                                next
                        } else{
                                flow.null[i]<-TRUE
                                stations<-stations+1
                                stations.id[i]<-list(id)
                        }
                }else{

                        flow.null[i]<-TRUE
                        stations<-stations+1
                        stations.id[i]<-NAflow.idx$id[i]
                        result.station<-readNWISsite(as.character(NAflow.idx$id[i]))

                        NAflow.idx$areagross[i]<-round(result.station$drain_area_va*2.589988,0) #mi2 to km2


                }

                print(paste(stations,"/",n))
        }

        #Maximum Distance Print
        print(paste("Farthest station:",network.sorted$distance.km[i]," km."))

        #Remove all the invalid stations
        NAflow.idx<-NAflow.idx[!is.na(match(NAflow.idx$id,stations.id))==T,]

        #Transform country in a character
        NAflow.idx$country<-as.character(NAflow.idx$country)

        #Storage the information
        NAflow.idx<<-NAflow.idx

        point.location<<-point.location
}




#' Find Gauge Stations (EC)
#'
#' Find the closest stations n from the HYDAT database based on latitude and longitude
#' @param lat   Latitude in decimal degrees
#' @param long  Longitude in decimal degrees (in Canada is negative)
#' @param n     Amount of station to be reviewed
#' @export
ECf.find<-function(lat=49.6, lon=-114.5, n=3){
        #lat=63.921190; long=-135.506710; n=200
        #lat=58.204576; long=-103.711064; n=5
        library("RODBC") #Database Access
        library("HYDAT") #Hydat Access
        library("geosphere") #Distance from coordinates
        library("hydroTSM")
        library("lattice")

        # Windows only (default path to EC Data Explorer)
        hydatPath <- 'c:/program files (x86)/EC/ECdataexplorer/database/hydat.mdb'

        dbc <- odbcConnectAccess( hydatPath )

        #Include the interesting point
        point.location<-c(Longitude=lon,Latitude=lat)

        # path to EC data explorer
        network <- ReadHydex(
                conn = dbc
        )

        # close DB connection
        odbcClose(dbc)

        #Removing regulated stations
        regulated<-is.na(network$regulated)
        network.unregulated<-network[regulated,]

        #Obtain coordiantes
        station.coordinates<-cbind(network.unregulated$longitude,network.unregulated$latitude)

        #Obtain distance from interesting point to Gauge stations. Info is passed to Km and rounded.
        distance.km<-round(distVincentySphere(point.location,station.coordinates)/1000,2)

        #Including distance in the network file
        network.unregulated<-cbind(network.unregulated,distance.km)

        #Sorted unregulated stations
        network.unregulated.sorted<-network.unregulated[order(distance.km),]

        #First n stations
        ECflow.idx<-network.unregulated.sorted
        selected.id<-ECflow.idx$id

        #Initial Values to be transfer
        stations.id<-list()
        flow.null<-list()

        # i is the counter for the station selected
        i<-0

        #station is the amount of stations selected
        stations<-0
        while(stations<n){

                i<-i+1

                # open database connection
                dbc <- odbcConnectAccess(hydatPath)
                data <- ReadHydat ( conn = dbc, stationID = selected.id[i] )

                # close database connection
                odbcClose(dbc)

                # display structure of returned data object

                daily<-data$flow$daily  #Daily Information
                name<-data$metadata$name #Station Name
                id<-data$metadata$id
                #print(i)
                if (is.null(daily)==TRUE){
                        flow.null[i]<-FALSE
                        next
                } else{
                        flow.null[i]<-TRUE
                        stations<-stations+1
                        stations.id[i]<-list(id)
                }

                print(paste(stations,"/",n))
        }

        #Maximum Distance Print
        print(paste("Farthest station:",network.unregulated.sorted$distance.km[i]," km."))
        #Call the function ECf.add

        ECflow.idx<<-ECflow.idx[!is.na(match(ECflow.idx$id,stations.id))==T,]

        point.location<<-point.location
}


#' Add Gauge Stations (EC)
#'
#' Add one specific station to the work database
#' @param stations.id Station web id from EC
#' @export

ECf.add<-function(x=stations.id, monthly.zero=NA){

        #x<-ECflow.idx$id

        library("RODBC") #Database Access
        library("HYDAT") #Hydat Access
        library("hydroTSM")
        library("geosphere") #Distance from coordinates
        library("lattice")
        library("zoo")

        stations.names<-list(NULL)
        stations.metadata<-list(NULL)
        flow.daily<-list(NULL)
        flow.peak<-list(NULL)
        flow.annualmaxima<-list(NULL)
        flow.annualminima<-list(NULL)
        flow.monthly<-list(NULL)
        flow.monthly.table<-list(NULL)
        flow.annual<-list(NULL)
        monthly.ts<-list(NULL)

        # Windows only (default path to EC Data Explorer)
        hydatPath <- 'c:/program files (x86)/EC/ECdataexplorer/database/hydat.mdb'

        #station is the amount of stations selected

        for (i in 1:length(x)){
                #i<-2
                # open database connection
                dbc <- odbcConnectAccess(hydatPath)

                data <- ReadHydat ( conn = dbc, stationID = x[i] )

                # close database connection
                odbcClose(dbc)

                # display structure of returned data object

                daily<-data$flow$daily  #Daily Information
                name<-data$metadata$name #Station Name
                id<-data$metadata$id

                print(paste("Stations N",i,": ",x[i]," / ",name))

                #Daily Values
                flow<-zoo(daily$value,daily$date)

                stations.names[i]<-list(name)
                stations.metadata[i]<-list(data$metadata)
                flow.daily[i]<-list(flow)

                #monthly matrix
                monthly.ts[i]<-list(daily2monthly.V2(x = flow,FUN = mean, na.rm=TRUE,max.miss=5))

                #If monthly value = zero is considered NA
                if ( is.na(monthly.zero)){
                        monthly.ts[[i]][monthly.ts[[i]]==0]=NA
                }

                flow.monthly.table[i]<-list(zoo2matrix(monthly.ts[[i]]))

                #monthly resume

                flow.monthly[i]<-list(colMeans(flow.monthly.table[[i]], na.rm=TRUE))

                #annual flow
                days.months<-c(31,28.25,31,30,31,30,31,31,30,31,30,31)
                annual.average<-sum(flow.monthly[[i]]*days.months)/365.25

                flow.annual[i]<-list(annual.average)

                #Annual peak - Definition
                flow.annualpeak<-data$flow$instantaneous.maxima

                #Annual peak - Time definition
                peak.time<-paste(flow.annualpeak$year,flow.annualpeak$month,
                                 flow.annualpeak$day,flow.annualpeak$hour,flow.annualpeak$minute)
                peak.time<-strptime(peak.time,"%Y %m %d")
                peak.data<-flow.annualpeak$value

                #Annual peak - Flag included in file
                peak.data<-cbind(peak.data,flow.annualpeak$flag)

                #Annual peak - DefinitionZoo creation
                flow.annualpeak<-zoo(x=peak.data,order.by = peak.time)

                #colnames(flow.annualpeak)<<-c("Data","Flag")

                #Annual peak - file presentation
                flow.peak[i]<-list(flow.annualpeak)

                #Annual maxima - Definition
                flow.annualmaxima.temp<-data$flow$annual.maxima

                #Annual maxima - Time definition
                annual.time<-paste(flow.annualmaxima.temp$year,flow.annualmaxima.temp$month,flow.annualmaxima.temp$day)
                annual.time<-strptime(annual.time,"%Y %m %d")
                annual.data<-flow.annualmaxima.temp$value

                #Annual maxima - Flag included in file
                annual.data<-cbind(annual.data,flow.annualmaxima.temp$flag)

                #Annual maxima - DefinitionZoo creation
                flow.annualmaxima.temp<-zoo(x=annual.data,order.by = annual.time)

                #Annual maxima - file presentation
                flow.annualmaxima[i]<-list(flow.annualmaxima.temp)




        }

        dbc <- odbcConnectAccess( hydatPath )

        # path to EC data explorer
        network <- ReadHydex(
                conn = dbc,
        )

        # close DB connection
        odbcClose(dbc)

        #list names
        names(stations.names)<-make.names(x)
        names(stations.metadata)<-make.names(x)
        names(flow.daily)<-make.names(x)
        names(flow.peak)<-make.names(x)
        names(flow.annualmaxima)<-make.names(x)
        names(flow.monthly.table)<-make.names(x)
        names(flow.monthly)<-make.names(x)
        names(flow.annual)<-make.names(x)
        names(monthly.ts)<-make.names(x)

        ECflow.db<-list(stations.metadata,flow.daily, monthly.ts, flow.monthly.table,flow.monthly, flow.annual, flow.annualmaxima, flow.peak )
        names(ECflow.db)<-c("index","daily.ts","monthly.ts","monthly.table","monthly.avg","annual.avg","annual.maxima","annual.peak")


        attr(ECflow.db,"Latitude")<-point.location[2]
        attr(ECflow.db,"Longitude")<-point.location[1]

        ECflow.db<<-ECflow.db

        #prepare the index again
        db2idx()

        save(ECflow.db,file=paste("ECflow ",length(x)," Elem.db.RData",sep=""),ascii=F)

}

#' Add Gauge Stations (EC&USGS)
#'
#' Add one specific station to the work database
#' @param stations.id Station web id from EC & USGS
#' @export

NAf.add<-function(x=stations.id, monthly.zero=NA){

        #x<-NAflow.idx$id[[28]]

        library("RODBC") #Database Access
        library("HYDAT") #Hydat Access
        library("hydroTSM")
        library("geosphere") #Distance from coordinates
        library("lattice")
        library("zoo")


        stations.metadata<-list(NULL)
        flow.daily<-list(NULL)
        flow.peak<-list(NULL)
        flow.annualmaxima<-list(NULL)
        flow.annualminima<-list(NULL)
        flow.monthly<-list(NULL)
        flow.monthly.table<-list(NULL)
        flow.annual<-list(NULL)
        monthly.ts<-list(NULL)

        # Windows only (default path to EC Data Explorer)
        hydatPath <- 'c:/program files (x86)/EC/ECdataexplorer/database/hydat.mdb'

        #station is the amount of stations selected

        for(i in 1:length(x)){

                if(nchar(x)[i]<=7){ #CANADA

                        #i<-1
                        # open database connection
                        dbc <- odbcConnectAccess(hydatPath)

                        data <- ReadHydat ( conn = dbc, stationID = x[i] )

                        # close database connection
                        odbcClose(dbc)

                        # display structure of returned data object

                        daily<-data$flow$daily  #Daily Information
                        name<-data$metadata$name #Station Name
                        id<-data$metadata$id

                        print(paste("Stations N",i,": ",x[i]," / ",name))
                        data$metadata$country<-"CA"

                        #Daily Values
                        flow<-zoo(daily$value,daily$date)


                        stations.metadata[i]<-list(data$metadata)
                        flow.daily[i]<-list(flow)

                        #monthly matrix
                        monthly.ts[i]<-list(daily2monthly.V2(x = flow,FUN = mean, na.rm=TRUE,max.miss=5))

                        #If monthly value = zero is considered NA
                        if ( is.na(monthly.zero)){
                                monthly.ts[[i]][monthly.ts[[i]]==0]=NA
                        }

                        flow.monthly.table[i]<-list(zoo2matrix(monthly.ts[[i]]))

                        #monthly resume

                        flow.monthly[i]<-list(colMeans(flow.monthly.table[[i]], na.rm=TRUE))

                        #annual flow
                        days.months<-c(31,28.25,31,30,31,30,31,31,30,31,30,31)
                        annual.average<-sum(flow.monthly[[i]]*days.months)/365.25

                        flow.annual[i]<-list(annual.average)

                        #Annual peak - Definition
                        flow.annualpeak<-data$flow$instantaneous.maxima

                        #Annual peak - Time definition
                        peak.time<-paste(flow.annualpeak$year,flow.annualpeak$month,
                                         flow.annualpeak$day,flow.annualpeak$hour,flow.annualpeak$minute)
                        peak.time<-as.Date(strptime(peak.time,"%Y %m %d"))
                        peak.data<-flow.annualpeak$value

                        #Annual peak - Flag included in file
                        peak.data<-cbind(peak.data,flow.annualpeak$flag)

                        #Annual peak - DefinitionZoo creation
                        flow.annualpeak<-zoo(x=peak.data,order.by = peak.time)

                        #colnames(flow.annualpeak)<<-c("Data","Flag")

                        #Annual peak - file presentation
                        flow.peak[i]<-list(flow.annualpeak)

                        #Annual maxima - Definition
                        flow.annualmaxima.temp<-data$flow$annual.maxima

                        #Annual maxima - Time definition
                        annual.time<-paste(flow.annualmaxima.temp$year,flow.annualmaxima.temp$month,flow.annualmaxima.temp$day)
                        annual.time<-as.Date(strptime(annual.time,"%Y %m %d"))
                        annual.data<-flow.annualmaxima.temp$value

                        #Annual maxima - Flag included in file
                        annual.data<-cbind(annual.data,flow.annualmaxima.temp$flag)

                        #Annual maxima - DefinitionZoo creation
                        flow.annualmaxima.temp<-zoo(x=annual.data,order.by = annual.time)

                        #Annual maxima - file presentation
                        flow.annualmaxima[i]<-list(flow.annualmaxima.temp)


                } else{#US
                        #i<-96
                        name<-NAflow.idx$name[i]
                        print(paste("Stations N",i,": ",x[i]," / ",name))

                        #daily Flows
                        flow.USGS<-importDVs(staid=as.character(NAflow.idx$id[i]),code="00060",stat="00003")
                        #fill gaps (USGS)
                        #flow.1<-fillMiss(flow.USGS,block = 30,pmiss = 50,model="trend",smooth=T)
                        #fix info
                        flow.2<-cleanUp(flow.USGS,task="fix")
                        #Zoo transformation
                        flow<-zoo(flow.2$val*0.02831685,as.Date(strptime(flow.2$dates,"%Y-%m-%d")))

                        stations.metadata[i]<-list(as.list(NAflow.idx[i,1:13]))#without including distance

                        flow.daily[i]<-list(flow)

                        #monthly matrix
                        monthly.ts[i]<-list(daily2monthly.V2(x = flow,FUN = mean, na.rm=TRUE,max.miss=5))

                        #If monthly value = zero is considered NA
                        if ( is.na(monthly.zero)){
                                monthly.ts[[i]][monthly.ts[[i]]==0]=NA
                        }

                        flow.monthly.table[i]<-list(zoo2matrix(monthly.ts[[i]]))

                        #monthly resume

                        flow.monthly[i]<-list(colMeans(flow.monthly.table[[i]], na.rm=TRUE))

                        #annual flow
                        days.months<-c(31,28.25,31,30,31,30,31,31,30,31,30,31)
                        annual.average<-sum(flow.monthly[[i]]*days.months)/365.25

                        flow.annual[i]<-list(annual.average)
                        #i<-17
                        peak<-readNWISpeak(as.character(NAflow.idx$id[i]),convertType = F)

                        peak<-zoo(as.numeric(peak$peak_va)*0.02831685, as.Date(strptime(peak$peak_dt,"%Y-%m-%d")))

                        if(length(peak)==0){ #If there is no peak then flow peak is NA

                                flow.peak[i]<-list(NA)
                        }else{

                                #Correcting to peak to have just one maximum in a year!!
                                peak<-aggregate(peak,year,max)
                                time(peak)<-as.Date(date_decimal(time(peak)))

                                flow.peak[i]<-list(peak)

                        }

                        #Date need to be corrected

                        miss.days<-aggregate(flow,year,length)>360 #more than 360 days

                        miss.info<-aggregate(flow,year,function(x)sum(is.na(x)))<=5 #less than 5 days missing

                        annual.maxima<-aggregate(flow,year,max)[miss.days&miss.info]
                        #date correction in zoo
                        time(annual.maxima)<-as.Date(date_decimal(time(annual.maxima)))

                        flow.annualmaxima[i]<-list(annual.maxima)

                }

        }


        #list names

        names(stations.metadata)<-make.names(x)
        names(flow.daily)<-make.names(x)
        names(flow.peak)<-make.names(x)
        names(flow.annualmaxima)<-make.names(x)
        names(flow.monthly.table)<-make.names(x)
        names(flow.monthly)<-make.names(x)
        names(flow.annual)<-make.names(x)
        names(monthly.ts)<-make.names(x)

        NAflow.db<-list(stations.metadata,
                        flow.daily,
                        monthly.ts,
                        flow.monthly.table,
                        flow.monthly,
                        flow.annual,
                        flow.annualmaxima,
                        flow.peak )
        names(NAflow.db)<-c("index","daily.ts","monthly.ts","monthly.table","monthly.avg","annual.avg","annual.maxima","annual.peak")


        attr(NAflow.db,"Latitude")<-point.location[2]
        attr(NAflow.db,"Longitude")<-point.location[1]

        NAflow.db<<-NAflow.db

        #prepare the index again
        #db2idx()

        save(NAflow.db,file=paste("NAflow ",length(x)," Elem.db.RData",sep=""),ascii=F)

}


#' Obtain the index file from the flow database
#'
#' Obtain the index file from the flow database
#' @export
db2idx<-function(){
        library("geosphere") #Distance from coordinates

        if (exists("ECflow.db")==TRUE){


                point.location<-c(attr(ECflow.db,"Longitude"),attr(ECflow.db,"Latitude"))

                ECflow.idx.tmp<-as.data.frame(matrix(unlist(ECflow.db$index),ncol = 12,byrow = T),stringsAsFactors = F)
                names(ECflow.idx.tmp)<-names(ECflow.db$index[[1]])

                ECflow.idx.tmp[,4:7]<-sapply(ECflow.idx.tmp[,4:7],as.numeric)

                #Obtain coordiantes
                station.coordinates<-cbind(ECflow.idx.tmp$longitude,ECflow.idx.tmp$latitude)

                #Obtain distance from interesting point to Gauge stations. Info is passed to Km and rounded.
                distance.km<-round(distVincentySphere(point.location,station.coordinates)/1000,2)

                #Including distance in the network file
                ECflow.idx<<-cbind(ECflow.idx.tmp,distance.km)

                point.location<<-point.location
        }

        if (exists("NAflow.db")==TRUE){


                point.location<-c(attr(NAflow.db,"Longitude"),attr(NAflow.db,"Latitude"))

                NAflow.idx.tmp<-as.data.frame(matrix(unlist(NAflow.db$index),ncol = 13,byrow = T),stringsAsFactors = F)
                names(NAflow.idx.tmp)<-names(NAflow.db$index[[1]])

                NAflow.idx.tmp[,4:7]<-sapply(NAflow.idx.tmp[,4:7],as.numeric)

                #Obtain coordiantes
                station.coordinates<-cbind(NAflow.idx.tmp$longitude,NAflow.idx.tmp$latitude)

                #Obtain distance from interesting point to Gauge stations. Info is passed to Km and rounded.
                distance.km<-round(distVincentySphere(point.location,station.coordinates)/1000,2)

                #Including distance in the network file
                NAflow.idx<<-cbind(NAflow.idx.tmp,distance.km)

                point.location<<-point.location


        }
}



#' Select stations which have n amount minimum amount of info
#'
#' Select stations which have n amount minimum amount of info
#' @param n     Minimun amount of years
#' @export
ECf.select.min<-function(n=0){mwi[mwi$Min<n,]$ID}

#' Remove the information from the work database
#'
#' Remove the information from the work database
#' @param x     Vector with the amount to be removed
#' @export
ECf.rm<-function(x=NULL){
        if(exists("ECflow.db")==TRUE){

                ECflow.db$index<<-ECflow.db$index[!x]
                ECflow.db$daily.ts<<-ECflow.db$daily.ts[!x]
                ECflow.db$monthly.ts<<-ECflow.db$monthly.ts[!x]
                ECflow.db$monthly.avg<<-ECflow.db$monthly.avg[!x]
                ECflow.db$monthly.table<<-ECflow.db$monthly.table[!x]
                ECflow.db$annual.avg<<-ECflow.db$annual.avg[!x]
                ECflow.db$annual.maxima<<-ECflow.db$annual.maxima[!x]
                ECflow.db$annual.peak<<-ECflow.db$annual.peak[!x]


                ECflow.idx<<-ECflow.idx[!t(x),]
                #         ECf.analysis()
        }

        if(exists("NAflow.db")==TRUE){

                NAflow.db$index<<-NAflow.db$index[!x]
                NAflow.db$daily.ts<<-NAflow.db$daily.ts[!x]
                NAflow.db$monthly.ts<<-NAflow.db$monthly.ts[!x]
                NAflow.db$monthly.avg<<-NAflow.db$monthly.avg[!x]
                NAflow.db$monthly.table<<-NAflow.db$monthly.table[!x]
                NAflow.db$annual.avg<<-NAflow.db$annual.avg[!x]
                NAflow.db$annual.maxima<<-NAflow.db$annual.maxima[!x]
                NAflow.db$annual.peak<<-NAflow.db$annual.peak[!x]


                NAflow.idx<<-NAflow.idx[!t(x),]
                #         ECf.analysis()
        }
}

#' Prepare runoff analysis
#'
#' Prepare runoff analysis
#' @export
ECf.analysis<-function(){
        library("hydroTSM")
        library("lattice")
        library("calibrate")
        library("matrixStats")

        #Patching for northamerican analysis
        if(exists("NAflow.db")==TRUE){

                ECflow.db<-NAflow.db
                ECflow.idx<-NAflow.idx
                print("North-American DB")

        }else{
                print("Canadian DB")
        }

        #Monthly Information Compilation
        monthly.data<-data.frame(t(data.frame(ECflow.db$monthly.avg,stringsAsFactors = F)))

        #Mean Annual Runoff
        MAR<-data.frame(t(data.frame(ECflow.db$annual.avg)))*86400*365.25/(ECflow.idx$areagross*1e6)*1000
        colnames(MAR)<-"MAR_mm"

        #Daily flows Comp
        flow.daily.comp<-do.call(merge,ECflow.db$daily.ts)

        #Monthly flow Comp
        flow.monthly.comp<-do.call(merge,ECflow.db$monthly.ts)

        #Monthly dwi Table
        mwi<-lapply(ECflow.db$monthly.table,function(i){
                colCounts(!is.na(i))
        })

        mwi<-matrix(data=unlist(mwi),byrow = TRUE, nrow=(length(mwi)),ncol=12)

        mwi<-as.data.frame(mwi)

        #Incliding min in mwi table
        min.mwi<-apply(mwi[c(1:12)],1,function(x){min(x)})

        #Incliding max in mwi table
        max.mwi<-apply(mwi[c(1:12)],1,function(x){max(x)})

        #Including Avg
        monthly.avg.mwi<-round(rowMeans(mwi),0)

        #cbind min,max,avg and ID
        mwi<-cbind(ECflow.idx$id,ECflow.idx$name,mwi,min.mwi,max.mwi,monthly.avg.mwi)

        #Including names
        colnames(mwi)<-c("id","name",month.abb,"Min", "Max","Avg")

        #days with information
        f.dwi<-dwi(flow.daily.comp)

        #Monthly non dimensional
        days.months<-c(31,28.25,31,30,31,30,31,31,30,31,30,31)
        monthly.data.nd<-cbind(ECflow.idx$name,monthly.data*days.months/rowSums(monthly.data*days.months))
        names(monthly.data.nd)<-c("Station.Name",month.abb)

        #outputs
        ECflow.syn<-list(MAR,flow.daily.comp,flow.monthly.comp, monthly.data, monthly.data.nd,mwi,f.dwi)
        names(ECflow.syn)<-c("MAR.table","daily.comp.ts","monthly.comp.ts", "monthly.avg.table","monthly.avg.nd.table","mwi.table","dwi.table")

        if(exists("NAflow.db")==TRUE){

                NAflow.syn<<-ECflow.syn


        }else{
                ECflow.syn<<-ECflow.syn
        }




        # dwi for matrixplot
        dwi.matrixplot<-ECflow.syn$dwi.table
        names(dwi.matrixplot)<-paste(ECflow.idx$name,ECflow.idx$id,sep=" - ")

        #reverse the rows (stations) Clossest station in the upper part of the graph
        dwi.matrixplot<-dwi.matrixplot[,rev(names(dwi.matrixplot))]
        #Matrixplot
        matrixplot(dwi.matrixplot, aspect="fill",ColorRamp = "Days")
}

#'Export the results from analysis
#'@param filename Excel filename to export the results
#'@export
ECf.out<-function(filename="output.xlsx",FA.results=FALSE,Wat.results=FALSE){

        library("xlsx")
        library("zoo")


        if(exists("NAflow.db")==TRUE){

                ECflow.idx<-NAflow.idx
                ECflow.syn<-NAflow.syn
                ECflow.db<-NAflow.db
                ECflow.FA<-NAflow.FA

        }

        #Site data.frame
        site.df<-data.frame(longitude=point.location[1],
                            latitude=point.location[2],
                            Coast.dist<-coast.dist(longitude = point.location[1],latitude = point.location[2]),
                            elev<-GE.elev(longitude = point.location[1],latitude = point.location[2]))
        names(site.df)<-c("longitude", "latitude","coast.dist","elev")
        row.names(site.df)<-c("site")

        #Java Garbage Collector
        jgc <- function()
        {
                .jcall("java/lang/System", method = "gc")
        }


        #Saving files
        write.xlsx2(ECflow.idx,filename,sheetName = "Gauge Network")

        mapply(write.xlsx2,x=ECflow.db$monthly.table,
               file=filename,
               sheetName=gsub("X","",names(ECflow.db$monthly.table)),
               append=T)
        jgc()

        #If Frequency analysis is asked
        if (FA.results==TRUE){
                write.xlsx2(ECflow.FA$FA.nd.table,filename,sheetName = "Freq.Analysis.nd",append = T)
                jgc()
                write.xlsx2(ECflow.FA$FA.table,filename,sheetName = "Freq.Analysis",append = T)
        }

        jgc()
        write.xlsx2(ECflow.syn$MAR.table,filename,sheetName = "MAR",append = T)
        jgc()
        write.xlsx2(ECflow.syn$monthly.avg.table,filename,sheetName = "monthly.avg",append = T)
        jgc()
        write.xlsx2(ECflow.syn$monthly.avg.nd.table,filename,sheetName = "monthly.nd.avg",append = T)
        jgc()
        write.xlsx2(ECflow.syn$monthly.comp.ts,filename,sheetName = "monthly.db",append = T)
        jgc()
        write.xlsx2(ECflow.syn$mwi.table,filename,sheetName = "MWI",append = T)
        if(Wat.results==TRUE){
                write.xlsx2(watershed.result,filename,sheetName = "Watershed",append = T)
        }

        jgc()
        write.xlsx2(site.df,filename,sheetName = "Site",append=T)
        jgc()
        #If Frequency analysis is asked
        if (FA.results==TRUE){
                write.xlsx2(ECflow.FA$FA.review.table,filename,sheetName = "Synthesis",append = T)
        }

}

#'Patch information between gauge stations
#'@param to.be.patched = row index from ECflow.idx of the station to be patched
#'@param with = row index from ECflow.idx of the station to be patched with
#'@param poly.n = polynomial degree for the fitting curve
#'@export
ECf.patch<-function(to.be.patched=2,with=4, poly.n=0.75){
        library("ggplot2")
        library("copula")
        library("proto")
        library("grid")

        #                 to.be.patched<-1
        #                 with<-2
        #
        print(paste(ECflow.idx$name[with],"->",ECflow.idx$name[to.be.patched]))


        time_ts<-time(ECflow.syn$daily.comp.ts)

        y<-ECflow.syn$daily.comp.ts[,to.be.patched]
        x<-ECflow.syn$daily.comp.ts[,with]

        x_y<-data.frame(x,y)

        x_y<-x_y[complete.cases(x_y),]

        #initial variable
        y2<-y

        #Polynomial regression
        if(poly.n>=1){

                model<-lm(x_y$y~poly(x_y$x,poly.n,raw=TRUE))

                y2[is.na(y)]<-polynEval(model$coefficients,x[is.na(y)])

                #R^2
                r2<-summary(model)$r.squared

        } else{
                model<-loess(x_y$y~x_y$x,span=poly.n)
                #just path, when there is no information
                y2[is.na(y)]<-predict(model,x[is.na(y)])

                #R^2
                ss.dist <- sum(scale(x_y$y, scale=FALSE)^2)
                ss.resid <- sum(resid(model)^2)

                r2<-1-ss.resid/ss.dist

        }

        #remove values lower than zero
        y2[y2<0]<-0

        #removing extremes ranges -> Interpolation not extrapolation

        y2[time(y2)<row.names(x_y)[1]]<-NA # Remove information before the arrangement
        y2[time(y2)>row.names(x_y)[nrow(x_y)]]<-NA # Remove information after the arrangement


        #include missing values from the original series
        y2[is.na(y2)]<-y[is.na(y2)]




        time(y[complete.cases(y)][length(y[complete.cases(y)])])


        #Definition of start and end
        start<-min(which(row.names(x_y)[1]==time(y)), #Define the min between the common point ...
                   which(time_ts==time(y[complete.cases(y)][1]))) #and the first point of the original time series


        end<-max(which(row.names(x_y)[nrow(x_y)]==time(y)),#Define the max between the common point ...
                 which(time_ts==time(y[complete.cases(y)][length(y[complete.cases(y)])])))#and the last point of the original time series


        #dataframe for graphs
        y2.df<-data.frame(time_ts[start:end],
                          coredata(y[start:end]),coredata(y2[start:end]))
        names(y2.df)<-c("Time","y","y2")


        #Tendency graph
        x.predict<-seq(from = min(x,na.rm = T),
                       to = max(x,na.rm=T),
                       length.out = 1000)
        if(poly.n>=1){

                reg.df<-data.frame(x=x.predict,
                                   y=polynEval(model$coefficients,x.predict))
        }else{
                reg.df<-data.frame(x=x.predict,
                                   y=predict(model,x.predict))

        }




        #Monthly graph
        monthly.y2<-daily2monthly.V2(x = y2,FUN = mean, na.rm=TRUE,max.miss=5)/(ECflow.idx$areagross[to.be.patched])
        monthly.y<-daily2monthly.V2(x = y,FUN = mean, na.rm=TRUE,max.miss=5)/(ECflow.idx$areagross[to.be.patched])
        monthly.x<-daily2monthly.V2(x = x,FUN = mean, na.rm=TRUE,max.miss=5)/(ECflow.idx$areagross[with])

        monthly.xy<-merge(monthly.x,monthly.y,all = T)
        monthly.xy<-monthly.xy[complete.cases(monthly.xy),]
        monthly.xy<-data.frame(coredata(monthly.xy))

        monthly.xy2<-merge(monthly.x,monthly.y2,all = T)
        monthly.xy2<-monthly.xy2[complete.cases(monthly.xy2),]
        monthly.xy2<-data.frame(coredata(monthly.xy2))
        names(monthly.xy2)

        r2.x<-0.2*(max(x_y$x)-min(x_y$x))+min(x_y$x)
        r2.y<-0.8*(max(x_y$y)-min(x_y$y))+min(x_y$y)

        r2.df<-data.frame(x=r2.x,y=r2.y,text=paste("r2=",signif(r2,3)))

        Fig1<-ggplot(x_y)+
                geom_point(aes(x=x,y=y),size=0.1)+
                labs(y=paste(ECflow.idx$name[to.be.patched],"[m3/s]"),
                     x=paste(ECflow.idx$name[with],"[m3/s]"),
                     title="Daily - Patching Relationship - Linear Scale")+
                theme(plot.title=element_text(size=20,vjust=2))+
                geom_path(data=reg.df,aes(x=x,y=y),color="red",size=1)+
                geom_text(data=r2.df,mapping=aes(x=x,y=y,label=text))

        Fig2<-ggplot(x_y)+
                geom_point(aes(x=x,y=y),size=0.1)+
                labs(y=paste(ECflow.idx$name[to.be.patched],"[m3/s]"),
                     x=paste(ECflow.idx$name[with],"[m3/s]"),
                     title="Daily - Patching Relationship - Logarithmic Scale")+
                theme(plot.title=element_text(size=20,vjust=2))+
                geom_path(data=reg.df,aes(x=x,y=y),color="red",size=1)+
                geom_text(data=r2.df,mapping=aes(x=x,y=y,label=text))+
                scale_y_log10()+
                scale_x_log10()+
                annotation_logticks()


        Fig3<-ggplot(monthly.xy,aes(x=monthly.x,y=monthly.y))+
                geom_point()+
                labs(y=paste(ECflow.idx$name[to.be.patched],"[m3/s/km2]"),
                     x=paste(ECflow.idx$name[with],"[m3/s/km2]"),
                     title="Monthly Avg. - Original Evaluation")+
                theme(plot.title=element_text(size=20,vjust=2))+
                stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)+
                geom_smooth(method=lm)



        Fig4<-ggplot(monthly.xy2,aes(x=monthly.x,y=monthly.y2))+
                geom_point()+
                labs(y=paste(ECflow.idx$name[to.be.patched],"- PATCHED [m3/s/km2]"),
                     x=paste(ECflow.idx$name[with],"[m3/s/km]"),
                     title="Monthly Avg. - Patched Evaluation")+
                theme(plot.title=element_text(size=20,vjust=2))+
                stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)+
                geom_smooth(method=lm)

        Fig5<-ggplot(y2.df)+
                geom_path(aes(x=Time, y=y2),color="red")+
                geom_path(aes(x=Time,y=y),color="blue")+
                labs(y=ECflow.idx$name[to.be.patched],
                     title=paste(ECflow.idx$name[to.be.patched],"- Corrected Time Series"))+
                theme(plot.title=element_text(size=20,vjust=2))

        #time series logarithmitc
        Fig6<-ggplot(y2.df)+
                geom_path(aes(x=Time, y=y2),color="red")+
                geom_path(aes(x=Time,y=y),color="blue")+
                labs(y=ECflow.idx$name[to.be.patched])+
                scale_y_log10()+
                annotation_logticks(sides="l")

        #Figure 7
        monthly2.ts<-daily2monthly(y2[start:end], FUN=mean)

        monthly2.nd.ts<-colMeans(zoo2matrix(monthly2.ts),na.rm = T)*c(31,28.25,31,30,31,30,31,30,30,31,30,31)/
                sum(colMeans(zoo2matrix(monthly2.ts),na.rm = T)*c(31,28.25,31,30,31,30,31,30,30,31,30,31))

        monthly1.ts<-daily2monthly(x[start:end], FUN=mean)

        monthly1.nd.ts<-colMeans(zoo2matrix(monthly1.ts),na.rm = T)*c(31,28.25,31,30,31,30,31,30,30,31,30,31)/
                sum(colMeans(zoo2matrix(monthly1.ts),na.rm = T)*c(31,28.25,31,30,31,30,31,30,30,31,30,31))


        monthly.nd.ts<-data.frame(months=rep(1:12,times = 2),
                                  nd.flow=c(monthly1.nd.ts,monthly2.nd.ts),
                                  Station=rep(c(ECflow.idx$name[with],ECflow.idx$name[to.be.patched]),each = 12),stringsAsFactors = F)


        Fig7<-ggplot(data=monthly.nd.ts)+
                geom_bar(aes(x=months, y=nd.flow,fill=Station),position=position_dodge(),stat="identity")+
                scale_x_discrete(breaks=1:12,limits=1:12,labels=month.abb)+
                labs(y="Non dimensional monthly distribution",
                     title="Monthly Avg. Distribution")+
                theme(plot.title=element_text(size=20,vjust=2),legend.position="bottom")




        grid.newpage()
        vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
        pushViewport(viewport(layout = grid.layout(3, 4)))
        print(Fig1,vp=vplayout(3,1))
        print(Fig2,vp=vplayout(3,2))
        print(Fig3,vp=vplayout(3,3))
        print(Fig4,vp=vplayout(3,4))

        print(Fig5,vp=vplayout(1,1:3))
        print(Fig6,vp=vplayout(2,1:3))
        print(Fig7,vp=vplayout(1:2,4))
        results<-list(y,y2[start:end],r2,Fig1,Fig2,Fig3,Fig4,Fig5,Fig6,Fig7)
        names(results)<-c("original.ts","patched.ts","r2","Fig1","Fig2","Fig3","Fig4","Fig5","Fig6","Fig7")

        results
}

#'Corrected the record adding patched information to ECflow.db
#'@param flow flow from the patched station (ECf.patch$cut.ts) or excel
#'@param idx row to be replaced with respect to ECflow.idx
#'@export
ECf.add.patched<-function(flow,idx){

        #monthly matrix
        monthly.ts<-daily2monthly.V2(x = flow,FUN = mean, na.rm=TRUE,max.miss=5)

        #adjustment for complete year
        flow.monthly.table<-zoo2matrix(monthly.ts)

        #monthly resume
        flow.monthly<-colMeans(flow.monthly.table, na.rm=TRUE)

        #annual flow
        days.months<-c(31,28.25,31,30,31,30,31,31,30,31,30,31)
        annual.average<-sum(flow.monthly*days.months)/365.25

        #Patching database
        #ECflow.db$index[[idx]]$id<<-paste(ECflow.db$index[[idx]]$id,"-P",sep="")
        ECflow.db$index[[idx]]$name<<-paste(ECflow.db$index[[idx]]$name,"- PATCHED",sep="")

        ECflow.db$daily.ts[[idx]]<<-flow
        ECflow.db$monthly.ts[[idx]]<<-monthly.ts
        ECflow.db$monthly.table[[idx]]<<-flow.monthly.table
        ECflow.db$monthly.avg[[idx]]<<-flow.monthly
        ECflow.db$annual.avg[[idx]]<<-annual.average

        db2idx()

}

#'Analysis of timeseries
#'@param ts time series
#'@param Area catchment area in km2
#'@export
ECf.ts<-function(ts,Area=NA){
        #Area=402.7
        #monthly matrix
        monthly.ts<-daily2monthly.V2(x = ts,FUN = mean, na.rm=TRUE,max.miss=5)

        #adjustment for complete year
        flow.monthly.table<-zoo2matrix(monthly.ts)

        #monthly resume
        flow.monthly<-colMeans(flow.monthly.table, na.rm=TRUE)

        #annual flow
        days.months<-c(31,28.25,31,30,31,30,31,31,30,31,30,31)
        annual.average<-sum(flow.monthly*days.months)/365.25

        #monthly distribution
        monthly.dist<-(flow.monthly*days.months)/(annual.average*365.25)

        #Patching database
        if(is.na(Area)){
                results<-list(ts,monthly.ts,flow.monthly.table,flow.monthly,monthly.dist,annual.average)
                names(results)<-c("daily.ts","monthly.ts","monthly.table","monthly.avg","monthly.dist","annual.avg")
        } else{

                MAR<-sum(flow.monthly*days.months)*86400/(Area*1e6)
                results<-list(ts,monthly.ts,flow.monthly.table,flow.monthly,monthly.dist,annual.average,MAR)
                names(results)<-c("daily.ts","monthly.ts","monthly.table","monthly.avg","monthly.dist","annual.avg","MAR")
        }



        results
}

#' Index table for EC Meteorology
#' @author Victor Munoz
#' @references USGS
#' @format Index table for USGS Meteorology
#' @name station.db.USGS
#' @usage data(station.db.USGS)
NULL


