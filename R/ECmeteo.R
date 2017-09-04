# Text lines for DB call --------------------------------------------------------------

#' @title Source URL for EC Meteorological Data (One Station)
#' @description This function provides the URL text for the historical data from a single meteorological station, sourced from Environment Canada.
#' @param stationid Station ID Number
#' @param year  Selected year of data
#' @param timeframe 1: Hourly, 2: Daily, 3: Monthly
#' @export
ECm.text<-function(stationid=1166,year=1956,timeframe=1){
        y<-NULL

        format<-"csv"
        y<-paste("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=",
                 format,"&stationID=",stationid,"&Year=",year,"&timeframe=",timeframe,
                 "&submit=Download+Data",sep="")


        y
}

#' @title Source URL for EC Climate Normals (One Station)
#' @description This function provides the URL text for the climate normals from a single meteorological station, sourced from Environment Canada.
#' @param stationid   Station ID Number
#' @param year.cn Start year for the different periods of the climate normals (1981-2010, 1971-2000, 1961-1990). Options: 1981, 1971, 1961
#' @export

ECm.text.n<-function(stationid=1174, year.cn=1981){

        Info<-ECm.id(stationid)
        y<-paste("http://climate.weather.gc.ca/climate_normals/bulk_data_e.html?ffmt=csv&lang=e&prov=BC&yr=",year.cn,"&stnID=",
                 stationid,"&climateID=",Info$"Climate Identifier",
                 "+++++++++++++&submit=Download+Data",sep="")
        y
}

# Station ID --------------------------------------------------------------
#' @title Station Description
#' @description This function summarizes the information about one meteorological station. Provides coordinates, elevation, and identifiers.
#' @param stationid Station ID Number
#' @export
ECm.id<-function(stationid=1166){
        library("RCurl") #Network interfase
        stationid<-stationid
        x<-getURL(url=ECm.text(stationid<-stationid,year=1900,timeframe=2))
        y<-read.csv(text=x,nrows=7,head=F)
        y<-t(y)
        colnames(y)<-y[1,]
        y<-y[2,]
        y<-as.data.frame(t(y))
        y[,"WebID"]<-stationid

        y$Latitude<-as.numeric(as.character(y$Latitude))
        y$Longitude<-as.numeric(as.character(y$Longitude))
        y$Elevation<-as.numeric(as.character(y$Elevation))
        y$WebID<-as.numeric(as.character(y$WebID))
        y

}
#' @title Climate Normals (One Station)
#' @description This function obtains the climate normals for one meteorological station. Sourced from Environment Canada.
#' @param stationid Station ID Number
#' @param year.cn Start year for the different periods of the climate normals (1981-2010, 1971-2000, 1961-1990). Options: 1981, 1971, 1961
#' @return a table of climate normals for the given station and period.
#' @export
ECm.normal<-function(stationid=1174, raw=FALSE, year.cn=1981){
        library("RCurl") #Network interfase

        #stationid<-2263
        #stationid<-1174
        x<-getURL(url=ECm.text.n(stationid, year.cn),async = TRUE)
        if(nchar(x)<=700){
                NULL
        }else{
                #print(ECm.id(stationid))
                #print(ECm.text.n(stationid))
                y0<-read.csv(text=x,nrows=1,skip=3)
                #print (y0)
                y1<-read.csv(text=x,skip=14,header = FALSE)
                #y1<<-y1
                Temp.location<-pmatch("Daily Average",y1[,1])

                MaxTemp.location<-pmatch("Daily Maximum",y1[,1])
                MinTemp.location<-pmatch("Daily Minimum",y1[,1])
                Rain.location<-match("Rainfall (mm)",y1[,1])
                Snow.location<-match("Snowfall (cm)",y1[,1])
                Pp.location<-match("Precipitation (mm)",y1[,1])
                Wind.location<-match("Speed (km/h)",y1[,1])
                Sunshine.location<-match("Bright Sunshine",y1[,1])+1
                if (is.na(match("Total Hours",y1[,1]))){Sunshine.location<-NA}
                Vpressure.location<-match("Average Vapour Pressure (kPa)",y1[,1])
                Humidity1.location<-match("Average Relative Humidity - 0600LST (%)",y1[,1])
                Humidity2.location<-match("Average Relative Humidity - 1500LST (%)",y1[,1])

                normal<-y1[c(Temp.location,
                             MinTemp.location,
                             MaxTemp.location,
                             Rain.location,
                             Snow.location,
                             Pp.location,
                             Wind.location,
                             Vpressure.location,
                             Sunshine.location,
                             Humidity1.location,
                             Humidity2.location),]

                #normal<-normal[-1]
                normal[,1]<-c("Avg.Temp.[degC]",
                              "Min.Temp.[degC]",
                              "Max.Temp.[degC]",
                              "Avg.Rainfall[mm]",
                              "Avg.Snowfall[cm]",
                              "Avg.Precipitation[mm]",
                              "Avg.Wind Speed[km/hr]",
                              "Avg.Vapour Pressure(kPa)",
                              "Avg.Total Sunshine[hrs]",
                              "Avg.Relat.Humidity - 0600LST(%)",
                              "Avg.Relat.Humidity - 1500LST(%)")
                names(normal)<-c("Parameter", month.abb,"Year","Code")

                #Every as a  number
                for (i in 2:14){normal[,i]<-as.numeric(as.character(normal[,i]))}

                #include dew point temperature

                min.temp<-normal[2,2:13]
                max.temp<-normal[3,2:13]
                min.RH<-normal[10,2:13]
                max.RH<-normal[11,2:13]
                min.DT<-round(RH2DT(Temp = min.temp,RH = min.RH),2)
                max.DT<-round(RH2DT(Temp = max.temp,RH = max.RH),2)

                min.DT<-data.frame(c("Dew Point Temp.- 0600LST[degC]",min.DT,NA,"E"))
                names(min.DT)<-c("Parameter", month.abb,"Year","Code")

                max.DT<-data.frame(c("Dew Point Temp.- 1500LST[degC]",max.DT,NA,"E"))
                names(max.DT)<-c("Parameter", month.abb,"Year","Code")


                normal<-rbind(normal,min.DT,max.DT)

                rownames(normal)<-1:nrow(normal)

                if(raw==TRUE){
                        results<-list(y0,normal,y1)
                        names(results)<-c("info","table","raw")
                } else{
                        results<-list(y0,normal)
                        names(results)<-c("info","table")


                }

                results
                #print(sum(!is.na(normal[,14])))

        }
}


# Tools to Locate Stations  -----------------------------------------------------------

#' @title Find Meteorological Stations from Environment Canada
#' @description Function for locating the nearest 'n' stations to a specified coordinate. Returns detailed list of Canadian stations that have available meteorological data in hourly or daily format.
#' @usage
#' ECm.find(lat, lon, n, timeframe)
#' @param lat   Latitude in decimal degrees.
#' @param lon  Longitude in decimal degrees. Note: Canadian longitudes are negative.
#' @param n     The number of stations to be located.
#' @param timeframe 1: Hourly, 2: Daily
#' @return a dataframe of 'n' stations
#' @examples
#' ## EX. 1: Find the closest 20 stations to 49.6, -114.5 that have hourly data.
#' ECm.find(lat=49.6, lon=-114.5, n=20, timeframe=1)
#'
#' ## EX. 2: Find the closest 10 stations to Ottawa, Canada that have daily data.
#' ECm.find(lat=45.42, lon = -75.70, n=10, timeframe=2)
#' @export
ECm.find<-function(lat=49.6, lon=-114.5, n=20, timeframe=1){
        library(RCurl)
        library(XML)
        library(geosphere) #Distance from coordinates
        library(maps)

        country<-map.where(database="world", lon, lat)

        if(!country%in%c("Canada", "NA")){
                warning('Specified coordinates are outside Canadian boundaries. Check coordinates.')
        }

        options(warn=-1)
        #data(station.db)
        index<-getURL("ftp://client_climate@ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees/Station Inventory EN.csv")

        file.name<-tempfile()
        save(index,file=file.name)

        station.db<-read.csv(file.name,skip=5,stringsAsFactors = F, skipNul = TRUE)

        names(station.db)<-c("Station.Name","Province","Climate.Identifier","WebID"," WMO.Identifier","TC.ID","Latitude","Longitude",
                             "Northing","Easting","Elevation","First.Year","Last.Year","HLY.First.Year","HLY.Last.Year",
                             "DLY.First.Year","DLY.Last.Year","MLY.First.Year","MLY.Last.Year")

        station.db<<-station.db

        point.location<<-c(lon,lat)
        names(point.location)<-(c("Longitude","Latitude"))
        #Obtain coordinates
        station.coordinates<-cbind(station.db$Longitude,station.db$Latitude)

        #Obtain distance from specified coordinates to stations. Distance is converted to km and rounded.
        station.distance<-round(distVincentySphere(point.location,station.coordinates)/1000,2)

        #Include distance in the network file
        selected.m.network<-cbind(station.db,station.distance)

        #Sorted unregulated stations
        selected.m.network<-selected.m.network[order(station.distance),]

        #Show the first n elements, if n=0 then show the complete network
        if(n==0){n<-nrow(station.db)}

        if(timeframe==2){selected.m.network<-selected.m.network[!is.na(selected.m.network$DLY.First.Year),]}

        if(timeframe==1){selected.m.network<-selected.m.network[!is.na(selected.m.network$HLY.First.Year),]}

        selected.m.network<<-head(selected.m.network,n)

        options(warn=0)

        #Display the selected network
        head(selected.m.network[,c(1:8,11,20)],n)

}


# Evaporation Tools -------------------------------------------------------

#' Estimate Evaporation from a EC information
#'
#' @param stationid   Latitude in decimal degrees
#' @param station.normal Normal table from ECn.normal()
#' @export
ECm.evap<-function(stationid=1174, station.normal=ECm.normal(stationid=1174)$table){
        library("zoo")
        library("Evapotranspiration")

        #stationid<-1174
        #station<-ECm.normal(stationid=stationid)
        daysmonth<-c(31,28,31,30,31,30,31,31,30,31,30,31)
        time<-seq(as.Date("2001/1/1"), by = "month", length.out = 12)

        #Define time series
        Date.daily<-seq(as.Date("2001/1/1"), by = "day", length.out = 365)
        Date.monthly<- as.yearmon(2001 + seq(0, 11)/12)
        J<-zoo(seq(from = 1, to=365),Date.daily)
        i<-1:12
        ndays<-zoo(daysmonth,Date.monthly)
        Tmax<-monthly2daily(zoo(unlist(station.normal[3,][2:13]),time))
        Tmin<-monthly2daily(zoo(unlist(station.normal[2,][2:13]),time))
        u2<-NULL
        uz<-monthly2daily(zoo(unlist(station.normal[7,][2:13]),time))
        Rs<-NULL
        n<-monthly2daily(zoo(unlist(station.normal[9,][2:13]/daysmonth),time))
        Cd<-NULL
        Precip<-monthly2daily(zoo(unlist(station.normal[6,][2:13]/daysmonth),time))
        Epan<-NULL
        RHmax<-NULL
        RHmin<-NULL
        Tdew<-monthly2daily(zoo(unlist(colMeans(subset(station.normal[12:13,],select=month.abb))),time))

        #Definition of constants
        Station.info<-ECm.id(stationid)
        data("constants")
        constants$Elev<-Station.info$Elevation
        constants$lat<-Station.info$Latitude
        constants$lat_rad<-Station.info$Latitude/180*pi
        constants$PA<-station.normal[6,14]

        constants<<-constants

        data2<-list(Date.daily,Date.monthly,J,i,ndays,Tmax,Tmin,u2,uz,Rs,n,Cd,Precip,Epan,RHmax,RHmin, Tdew)
        names(data2)<-c("Date.daily","Date.monthly","J","i","Ndays","Tmax","Tmin","u2","uz","Rs","n","Cd","Precip","Epan","RHmax","RHmin", "Tdew")

        #Morton CRAE Calculation
        class(data2)<-"MortonCRAE"
        funname<<-"MortonCRAE"
        data2<<-data2
        a1<-ET.MortonCRAE(data2,constants,est = "potential ET",solar = "sunshine hours",Tdew = TRUE)

        a2<-ET.MortonCRAE(data2,constants,est = "actual areal ET",solar = "sunshine hours",Tdew = TRUE)

        #Morton CRWE Calculation
        class(data2)<-"MortonCRWE"
        funname<<-"MortonCRWE"
        a3<-ET.MortonCRWE(data2,constants,est = "potential ET",solar = "sunshine hours",Tdew = TRUE)
        a4<-ET.MortonCRWE(data2,constants,est = "shallow lake ET",solar = "sunshine hours",Tdew = TRUE)

        #Table setting
        evap<-t(data.frame(round(a1$ET.Monthly,2),round(a2$ET.Monthly,2),round(a3$ET.Monthly,2),round(a4$ET.Monthly,2)))
        evap<-cbind(c("Morton-Potential ET[mm]","Morton-Actual areal ET[mm]","Morton-Potential[mm]", "Morton-Shallow lake[mm]"),evap)
        #rownames(evap)<-c("Morton-Potential ET[mm]","Morton-Actual areal ET[mm]","Morton-Potential[mm]", "Morton-Shallow lake[mm]")

        evap<-as.data.frame(evap,stringsAsFactors = FALSE)
        row.names(evap)<-1:4

        evap.annual<-t(data.frame(round(a1$ET.Annual,2),round(a2$ET.Annual,2),round(a3$ET.Annual,2),round(a4$ET.Annual,2)))

        evap<-cbind(evap,evap.annual,"E")
        colnames(evap)<-c("Parameter",month.abb,"Year","Code")

        #Every as a  number
        for (i in 2:14){evap[,i]<-as.numeric(as.character(evap[,i]))}

        station.normal<-rbind(station.normal,evap)
        station.normal

}

# Parameters from 1981-2010 Normals ----------------------------------------
#' Estimate Evaporation from a EC information
#'
#' @param lat    Latitude in decimal degrees
#' @param lon    Longitude in decimal degrees
#' @param n      Amount of station to be review
#' @export
ECm.normal.capture<-function(lat=49.645318,lon=-114.453425,n=5) {

        #find the complete data set
        ECm.find(lat=lat,lon=lon,n=0,timeframe = 1)

        #prepare a data set called selected.n.network just with daily info
        selected.n.network<<-selected.m.network#[!selected.m.network$HLY.First.Year=="N/A",]

        #remove the ranges
        selected.n.network<<-selected.n.network[,c(1:8,11)]
        n.stations<-nrow(selected.n.network)

        #Setup of the variables
        ECm.evaporation<<-NULL
        evaporation.stations<-NULL
        station.parameters<-NULL
        #define of counter for the i sequence through selected networw and
        # evap.inventory
        evap.inv<-0
        i<-0
        while (evap.inv<n){

                # for (i in 1:n.stations){
                i<-i+1

                normal<-ECm.normal(stationid = selected.n.network$WebID[i])

                #normal<-ECm.normal(1237)
                #Status management
                #definition
                status<-ECm.normal.avail(normal$table,out=TRUE)
                #integer value
                status.value<-1*grep ("T2", status)+
                        1*grep ("T3", status)+
                        1*grep ("P3", status)+
                        1*grep ("S", status)+
                        1*grep ("DW06", status)+
                        1*grep ("DW15", status)
                if (length(status.value)==0){status.value<-0}

                #matrix definition
                station.parameters[i]<-list(status)

                if (status.value!=6){
                        #Work in this part with ECm.normal.aval(normal, out=TRUE)
                        #match(c("T1 ","T2 "),a)
                        print(paste(selected.n.network$WebID[i],
                                    selected.n.network$Station.Name[i]))

                        ECm.normal.avail(normal$table,out=FALSE)

                        next
                }else{
                        evap.inv<-evap.inv+1
                        print(paste(selected.n.network$WebID[i],
                                    selected.n.network$Station.Name[i]))
                        ECm.normal.avail(normal$table,out=FALSE)
                        #Include comments of the quality of the info!
                        ECm.evaporation[evap.inv]<-list(ECm.evap(stationid = selected.n.network$WebID[i],station.normal = normal$table))
                        evaporation.stations[evap.inv]<-list(selected.n.network$WebID[i])
                }
                names(ECm.evaporation)<-evaporation.stations
                ECm.evaporation<<-ECm.evaporation

        }
        #leave the first i stations
        selected.n.network<<-head(selected.n.network,i)


        #including the meteorological info in the database
        b<-lapply(station.parameters,function(a){if(length(a)==0){""} else{a}})
        b<-as.data.frame(unlist(b))
        colnames(b)<-"Info"

        #define the amount of variable per station
        c<-lapply(b,function(a){
                a<-as.character(a)
                sapply(strsplit(a, "\\s+"), length)
        })
        c<-as.data.frame(unlist(c))
        colnames(c)<-"Parameters"

        #combine with the meteorological info
        selected.n.network<<-cbind(selected.n.network,c,b)

        #remove any row where there is no info
        selected.n.network<<-selected.n.network[!selected.n.network$Info=="",]

}
# Parameters from 1981-2010 Normals ----------------------------------------
#' Check availability of info
#'
#' @export
ECm.normal.avail<-function(a, out=FALSE){
        if (!is.null(a)){
                #a<-ECm.normal(1237)
                b<-as.logical(rowSums(a[,2:13],na.rm = FALSE))
                b[is.na(b)]<-FALSE
                b*1
                text<-c("T1","T2","T3","P1","P2","P3","W","VP","S","H06","H15","DW06","DW15")
                d<-text[b]
                d<-paste(d,collapse=' ')

                if(out==TRUE){d} else {message(d)}
        }
}

# Meteorological Tools ----------------------------------------------------


#' @title Download Station Information (One Station)
#' @description Function to capture the available monthly, daily or hourly meteorological information from one meteorological station. Used in \code{\link[Hydro]{ECm.capture.all}}.
#' @param stationid   Station ID Number
#' @param timeframe 1: Hourly, 2: Daily, 3: Monthly
#' @return a table consisting of all available meteorological data for one station.
#' @export
ECm.capture<-function(stationid=3985, timeframe=2){

        library(hydroTSM)
        library(RCurl)
        library(XML)

        options(warn=-1)

        opts <- list(
                fresh.connect = TRUE,
                forbid.reuse = TRUE,
                maxconnects=3
        )

        options(RCurlOptions = opts)

        #stationid=31087

        print(stationid)
        #Define a variable valid year
        if(timeframe==3){
                #Years from Daily DB
                webpage<-getURL(paste("http://climate.weather.gc.ca/climate_data/monthly_data_e.html?StationID=",
                                      stationid,sep=""))}

        if(timeframe==2){
                #Years from Daily DB
                webpage<-getURL(paste("http://climate.weather.gc.ca/climate_data/daily_data_e.html?StationID=",
                                      stationid,sep=""))}

        if(timeframe==1){
                #Years from Daily DB
                webpage<-getURL(paste("http://climate.weather.gc.ca/climate_data/hourly_data_e.html?StationID=",
                                      stationid,sep=""))}

        # doc = htmlParse(webpage, asText=TRUE)
        #
        # plain.text <- xpathSApply(doc, path = "//option", xmlValue)
        #
        # years.day<-unique(plain.text)
        # #Remove month and transform in numeric
        # years.day<-as.numeric(years.day[is.na((match(years.day,month.name)|match(years.day,as.character(1:12))))])
        # years.day<-c(min(years.day),max(years.day))
        #
        # #Correct in case of mistake in the years
        # if(max(years.day[1],years.day[2])>5000){years.day<-c(2000,2000)}

        stationid.db<-station.db[grep(paste0("\\b",stationid,"\\b"),station.db$WebID),]


        if(timeframe==1){
                years.avail<-as.list(rep(c(stationid.db$"HLY.First.Year": stationid.db$"HLY.Last.Year"),each=12))

                urls<-paste0("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv",
                             "&stationID=",stationid,"&Year=",years.avail,"&Month=",1:12,"&timeframe=",timeframe,
                             "&submit=Download+Data")
        }


        if(timeframe==2){
                years.avail<-as.list(stationid.db$"DLY.First.Year": stationid.db$"DLY.Last.Year")

                urls<-paste0("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv",
                             "&stationID=",stationid,"&Year=",years.avail,"&timeframe=",timeframe,
                             "&submit=Download+Data")
        }

        #URL for all the years
        #urls<-c(ECm.text(stationid,year=years.avail,timeframe))

        EC.info<-getURIAsynchronous(urls,.opts = list(verbose = TRUE),
                                    binary = rep(FALSE, length(urls)))

        closeAllConnections()

        EC.info<<-EC.info
        #Complete Table
        if(timeframe==1){
                #Save a temporal file
                tfile<-tempfile()
                save(EC.info,file=tfile)
                #Read lines
                csv.file<-readLines(tfile)

                #head(csv.file,20)
                skip.file<-grep("Date/Time",csv.file)[1]-4

                #Defile the line to skipe
                EC.table.4<-read.csv(text=EC.info,skip=skip.file,stringsAsFactors = F)

                if(ncol(EC.table.4)==25&names(EC.table.4)[1]=="Date.Time"){EC.table<-EC.table.4}else{

                        skip.file<-grep("Date/Time",csv.file)[1]-3

                        #Defile the line to skipe
                        EC.table.3<-read.csv(text=EC.info,skip=skip.file,stringsAsFactors = F)

                        if(ncol(EC.table.3)==25&names(EC.table.3)[1]=="Date.Time"){EC.table<-EC.table.3}else{

                                skip.file<-grep("Date/Time",csv.file)[1]-2
                                EC.table.2<-read.csv(text=EC.info,skip=skip.file,stringsAsFactors = F)

                                if(ncol(EC.table.2)==25&names(EC.table.2)[1]=="Date.Time"){EC.table<-EC.table.2}else{

                                        skip.file<-grep("Date/Time",csv.file)[1]-1
                                        EC.table.1<-read.csv(text=EC.info,skip=skip.file,stringsAsFactors = F)

                                        if(ncol(EC.table.1)==25&names(EC.table.1)[1]=="Date.Time"){EC.table<-EC.table.1}

                                }

                        }

                }


                #Select the table with more columns
                #tables.EC.table<-list(EC.table.1,EC.table.2,EC.table.3)
                #ncol.table<-c(ncol(EC.table.1),ncol(EC.table.2),ncol(EC.table.3))

                #EC.table<-tables.EC.table[[grep(max(ncol.table),ncol.table)]]

                #EC.table<-read.csv(text=EC.info,skip=15,stringsAsFactors = F)
                EC.table$Date.Time<-as.POSIXct(EC.table$Date.Time, format = "%Y-%m-%d %H:%M" )
                EC.table<-EC.table[!is.na.POSIXlt(EC.table$Date.Time),]
                num.param<-c(2,3,4,6,7,9,11,13,15,17,19,21,23)
        }

        if(timeframe==2){
                EC.table<-read.csv(text=EC.info,skip=24,stringsAsFactors = F)
                EC.table$Date.Time<-as.POSIXct(EC.table$Date.Time, format = "%Y-%m-%d" )
                EC.table<-EC.table[!is.na.POSIXlt(EC.table$Date.Time),]
                num.param<-c(2,3,4,6,8,10,12,14,16,18,20,22,24,26)

        }

        EC.table[,num.param]<-data.frame(sapply(EC.table[,num.param],as.numeric))

        options(warn=0)
        EC.table

}

#' @title Download Station Information
#' @description Function to capture the available monthly, daily or hourly meteorological information from the selected meteorological stations using \code{\link[Hydro]{ECm.find}}. Uses \code{\link[Hydro]{ECm.capture}} with the option to use multiple processors.
#' @usage ECm.capture.all(multicore, timeframe)
#' @param multicore True or false; State whether the function may utilize multicore capabilities for faster processing.
#' @param timeframe 1: Hourly, 2: Daily, 3: Monthly
#' @return a dataframe consisting of all available meteorological data for each station.
#' @examples
#' ## EX 1: Return all hourly data for previous selected 'n' stations.
#' Assume multicore capabilities.
#' ECm.capture.all(multicore = T, timeframe = 1)
#' @export
ECm.capture.all<-function(multicore=T,timeframe=2){
        q<-proc.time()
        if(multicore==T){
                library(parallel)
                cl<-makeCluster(detectCores())
                clusterExport(cl,c("station.db","timeframe"),
                              envir = environment())
                ECm.stations<-parLapply(cl,as.list(selected.m.network$WebID),fun=function(x)Hydro::ECm.capture(x,timeframe))
                stopCluster(cl)

        }  else{

                ECm.stations<-lapply(as.list(selected.m.network$WebID),FUN=function(x)Hydro:::ECm.capture(x,timeframe))
        }

        names(ECm.stations)<-selected.m.network$"Climate.Identifier"



        if(timeframe==2){
                ECm.stations.daily<<-ECm.stations
                filename<-paste0("EC Daily Canada Data - ",length(ECm.stations)," Stations.RData")
        }

        if(timeframe==1){
                ECm.stations.subdaily<<-ECm.stations
                filename<-paste0("EC Hourly Canada Data - ",length(ECm.stations)," Stations.RData")
        }

        save(ECm.stations, file=filename,ascii=FALSE, compress=TRUE)
        proc.time()-q

}

ECm.save<-function(){
        #setwd("//VAN-SVR0/Projects/01_SITES/Grassy Mountain/!020_Site_Wide_Data/Hydrology/Precipitation")
        #save(ECm.stations, file="EC Meteorological Stations Daily Data.RData",ascii=FALSE, compress=TRUE)
        #save(selected.m.network, file="Selected Meteorological Stations.RData",ascii=FALSE, compress=TRUE)
        #save(station.db, file="EC Meteorological DB.RData",ascii=FALSE, compress=TRUE)
        write.csv(selected.network, file="Selected stations.csv") #Meteorological information
        #write.csv(station.db, file="EC Meteorological DB.csv")
        write.csv(ECm.evaporation, file="Evaporation.csv")
        write.csv(ECm.annual.comp, file="Annual compilation.csv")
        write.csv(ECm.monthly.data, file="Monthly compilation.csv")
        write.csv(ECm.monthly.data.nd, file="Monthly non dimensional compilation.csv")
        write.csv(ECm.monthly.mwi, file="Monthly information available.csv")

        write.csv(ECm.annual.peak.table,file="Annual Daily Precipitation.csv")
        write.csv(ECm.st.totp.comp,file="Daily Precipitation.csv")
}

#' @title Meteorological Variable Names
#' @description Displays the meteorological variables that are available at the stations that can be used for input into \code{\link[Hydro]{ECm.prepare}}.
#' @usage ECm.variables(db)
#' @param db The database of station meteorological information returned from \code{\link[Hydro]{ECm.capture.all}}
#' @return a character vector of meteorological variables
#' @export
ECm.variables<-function(db=ECm.stations){


        nm<-names(db[[1]])

        except<-unique(c(grep("Date.time",nm),
                         grep("Date.Time",nm),
                         grep("Year",nm),
                         grep("Month",nm),
                         match("Day",nm),
                         grep("Time",fixed=T,nm)))


        nm[-c(except,grep(".Flag",nm))]
}



#' @title Prepare for Analysis
#' @description This function prepares the database for use in \code{\link[Hydro]{ECm.analysis}} based on the selected meteorological parameter by removing all other parameters from the database.
#' @usage ECm.prepare(db, Parameter.name)
#' @param db  The database of daily or hourly meteorological information obtained from \code{\link[Hydro]{ECm.capture.all}}.
#' @param Parameter.name The selected meteorological parameter name from \code{\link[Hydro]{ECm.variables}}.
#' @examples
#' ## EX 1: Prepare database for Mean Temperature and remove all other parameters
#' ECm.prepare(db=ECm.stations.daily, Parameter.name="Mean.Temp..Â.C.")
#' @export
ECm.prepare<-function(db=ECm.stations,Parameter.name="Total.Precip..mm."){
        #Simplyfy DB just with Temperature, PP, rainfall, snow and snow over ground
        #ECm.st.sim<-lapply(ECm.stations, function(i){i[c(1,10,16,18,20,22)]})
        #DB with just total pp

        #ECm.find(lat=point.location[2],lon=point.location[1],n)

        #sort the info based on ECm.find
        ECm.stations.red<-db[selected.m.network$Climate.Identifier]

        #Parameter.name<-"34"

        Parameter.id<-match(Parameter.name,names(db[[1]]))

        if(is.na(Parameter.id)){
                ECm.variables(db)

        }else{

                #For Total Precipitation column 20
                ECm.st.totp<-lapply(ECm.stations.red, function(i){i[c(1,Parameter.id)]})


                #REVIEW!!!!!!

                #removing null element
                y<-lapply(ECm.st.totp, is.null)
                y<-!unlist(y)
                ECm.st.totp<-ECm.st.totp[y]
                selected.m.network<-selected.m.network[y,]

                #Saving file in memory
                Parameter.file<-paste0(Parameter.name,"db")

                assign(Parameter.file,ECm.st.totp,envir = .GlobalEnv)

                selected.m.network<<-selected.m.network

        }

}


#' @title Meteorological Data Analysis
#' @description This function compiles meteorological information on different timescales for the selected parameter and presents data in graphical and tabular form.
#' Information is sourced from the database that was previously prepared by \code{\link[Hydro]{ECm.prepare}} or edited by \code{\link[Hydro]{ECm.rm}}. Data is processed on daily, monthly and annual timescales.
#' Daily information may be converted to monthly and annual data by sumation or averaging. Data is omitted if the minimum days in a year or month criteria is not fulfilled.
#' @usage ECm.analysis<-function(db, FUN.month, Min.days.annual, Max.miss.days.month, output, EC, matrix.start.year)
#' @param db the database from \code{\link[Hydro]{ECm.prepare}} that contains the information for the selected meteorological parameter
#' @param FUN.month function used to convert daily data into monthly and annual data. Options: sum, mean
#' @param Min.days.annual the minimum number of days in a year for the data to be included in the analysis. Suggested: 200-365
#' @param Max.days.month the maximum number of missing days in a month for the data to be included in the analysis. Suggested: 0-5
#' @param output the name of the returned database
#' @param EC specify if the original data was sourced from Environment Canada. If TRUE, function uses information from "selected.m.network" database
#' @param matrix.start.year the start year of the graph presenting available information. Does not affect tabular output.
#' @return Three returns:
#' 1. Graph summarizing years with data for every station. \cr
#' 2. Database of compiled meteorological information (see details below). \cr
#' 3. Summary of the stations analyzed with annual information. \cr
#'
#' Database Elements:
#' \itemize{
#' \item Daily.Comp - daily zoo object of available meteorological information.
#' \item Monthly.avg - monthly averages of available meteorological information.
#' \item Monthly.avg.nd - monthly averages presented as a percentage of the year (i.e. non dimensional).
#' \item Monthly.table - monthly information for every year and station.
#' \item Month.Comp - monthly zoo object of available meteorological information.
#' \item MWI - summary of the months with information for each station, supported with average and max/min.
#' \item Annual - annual information for every station.
#' \item Annual.comp - annual zoo object of available meteorological information.
#' \item Annual.Syn - similar table to function output. Describes \code{\link[nsRFA]{Lmoments}} and identifies the month of the min and max, and the rate (i.e. max/min) to describe the shape of hydrograph.
#' \item Max.Daily.data - presents the daily maximum per year for each station.
#' \item Max.Daily.table - presents the daily maximum per year for each station as a table.
#' }
#' Please note that data from incomplete years and months has been omitted in each of the database elements.
#' @examples
#' ## EX 1: Analyze the precipitation totals for EC stations. Consider a full year of data as 360 days, and a maximum of 5 missing days per month. Present the graph starting in the year 1980.
#' ECm.analysis(db=Total.Precip..mm.db, FUN.month=sum, Min.days.annual=360, Max.miss.days.month=5, output="station.syn", EC=TRUE, matrix.start.year=1980)
#'
#' @export
ECm.analysis<-function(db=ECm.st.totp,FUN.month=sum,
                       Min.days.annual=360,Max.miss.days.month=5,output="station.syn",EC=TRUE,matrix.start.year=1960){

        #db<-Total.Precip..mm.db
        library(hydroTSM)
        library(matrixStats)
        library(lubridate)
        library(reshape2)
        library(parallel)
        library(nsRFA)

        ECm.st.totp<-db


        #Preparation for parallel processing
        cl<-makeCluster(detectCores())
        clusterExport(cl,c("FUN.month","Min.days.annual","Max.miss.days.month"),
                      envir = environment())


        ECm.monthly.main<-parLapply(cl,ECm.st.totp,fun=function(x){
                Hydro::daily2monthly.V2(x = zoo(x[,2],x[,1]),
                                        FUN = FUN.month,max.miss=Max.miss.days.month)
        })

        ECm.monthly.main2<-do.call(merge,ECm.monthly.main)

        ECm.monthly.table<-parLapply(cl,ECm.monthly.main,fun=function(x){
                Hydro::zoo2matrix(x)
        })


        ECm.annual<-lapply(ECm.monthly.table,FUN=function(x){
                #x<-ECm.monthly.table[[2]]
                annual<-apply(x,1,FUN=function(y){
                        #y<-x[3,]

                        if((sum(!is.na(y)))==12){

                                annual.val<-NULL

                                if(identical(FUN.month,sum)){
                                        annual.val<-sum(y)}

                                if(identical(FUN.month,mean)){
                                        annual.val<-sum(y*c(31,28.25,31,30,31,30,31,31,30,31,30,31))/365.25}

                                if(identical(FUN.month,max)){
                                        annual.val<-max(y)}

                                if(identical(FUN.month,min)){
                                        annual.val<-min(y)}


                        }else{
                                annual.val<-NA
                        }
                        annual.val
                })

                #annual<<-annual

                zoo(annual, as.numeric(attr(annual,"names")))

        })

        ECm.annual.table<-do.call(merge,ECm.annual)

        ECm.lmoments<-lapply(ECm.annual,FUN=function(x){
                Lmoments(as.numeric(x))
        })

        ECm.lmoments.table<-do.call(rbind,ECm.lmoments)

        #monthly table
        ECm.monthly<-parLapply(cl,ECm.monthly.table,fun=function(x){
                colMeans(x, na.rm=TRUE)
        })
        ECm.monthly.data<-as.data.frame(do.call(rbind,ECm.monthly))

        ECm.monthly.data<-cbind(names(ECm.st.totp),ECm.monthly.data)

        names(ECm.monthly.data)<-c("Station.Name",month.abb)

        #Month with Information
        ECm.monthly.mwi<-parLapply(cl,ECm.monthly.table,fun=function(x){
                month.avgs<-matrixStats::colCounts(!is.na(x))
                month.min<-min(month.avgs)
                month.max<-max(month.avgs)
                month.avg<-round(mean(month.avgs),0)
                c(month.avgs,month.min,month.max,month.avg)

        })
        ECm.monthly.mwi<-as.data.frame(do.call(rbind,ECm.monthly.mwi))
        colnames(ECm.monthly.mwi)<-c(month.abb,"MWI.Min", "MWI.Max","MWI.Avg")


        #Include non dimensional precipitation
        ECm.monthly.data.nd<-round(ECm.monthly.data[,2:13]/rowSums(ECm.monthly.data[,2:13]),3)
        ECm.monthly.data.nd<-cbind(names(ECm.st.totp),ECm.monthly.data.nd)
        names(ECm.monthly.data.nd)<-c("Station.Name",month.abb)


        #Mean Annual Pp
        if(identical(FUN.month,sum)){MAP<-as.data.frame(rowSums(ECm.monthly.data[,2:13]))}

        if(identical(FUN.month,mean)){MAP<-as.data.frame(
                rowSums(c(31,28.25,31,30,31,30,31,31,30,31,30,31)*ECm.monthly.data[,2:13])/365.25)}

        if(identical(FUN.month,max)){MAP<-apply(ECm.monthly.data[,2:13],1,max)}

        if(identical(FUN.month,min)){MAP<-apply(ECm.monthly.data[,2:13],1,min)}

        if(EC==TRUE){
                #Annual Analysis
                ECm.annual.comp<-data.frame("WebID"=selected.m.network$WebID,
                                            "Station.Name"=selected.m.network$Station.Name,
                                            "Province"=selected.m.network$Province,
                                            "Longitude"=selected.m.network$Longitude,
                                            "Latitude"=selected.m.network$Latitude,
                                            "Annual"=MAP,
                                            "Elevation"=selected.m.network$Elevation,
                                            "Distance"=selected.m.network$station.distance,stringsAsFactors = F)
                names(ECm.annual.comp)<-c("WebID","Station.Name","Province","Longitude","Latitude","Annual","Elevation","Distance")
        } else{

                ECm.annual.comp<-data.frame(Annual=MAP,stringsAsFactors = F)

                names(ECm.annual.comp)[1]<-"Annual"

        }
        param<-parApply(cl,ECm.monthly.data.nd[,-1],1,FUN=function(x){
                x<-cbind(reshape2::melt(x))
                y<-rbind(x,x)

                y<-data.frame("month"=1:12,"two_month_pp"=zoo::rollsum(y$value,2)[1:12])

                min.2m<-match(min(y$two_month_pp),y$two_month_pp)
                max.2m<-match(max(y$two_month_pp),y$two_month_pp)

                rate.2m<-y$two_month_pp[min.2m]/y$two_month_pp[max.2m]

                data.frame("Min_2m"=min.2m,
                           "Max_2m"=max.2m,
                           "Rate_2m"=round(rate.2m,3))
        })

        param<-do.call(rbind,param)

        ECm.annual.comp<-cbind(ECm.annual.comp,param,ECm.lmoments.table)

        # Daily Max Analysis
        ECm.annual.peak.data<-parLapply(cl,ECm.st.totp,fun=function(i){

                Avail.years<-aggregate(x=i[,2],by = list(lubridate::year(i[,1])),FUN = function(x)sum(!is.na(x))>Min.days.annual)
                Max.years<-aggregate(x=i[,2],by = list(lubridate::year(i[,1])),FUN = function(x){
                        if(sum(!is.na(x))>0){max(x,na.rm = T)}else{NA}})

                Max.years<-as.data.frame(Max.years)

                #Clean the time series when a Avail Years is not feasible
                Max.years[!Avail.years[,2],2]<-NA
                names(Max.years)<-c("Years","Annual Peak")
                Max.years

        })


        ECm.annual.peak.zoo<-parLapply(cl,ECm.annual.peak.data,fun=function(i)zoo::zoo(i$`Annual Peak`,i$Years))

        ECm.annual.peak.table<-do.call(merge,ECm.annual.peak.zoo)
        ECm.annual.peak.table<-as.data.frame(ECm.annual.peak.table)

        ECm.st.totp.zoo<-parLapply(cl,ECm.st.totp,fun=function(x)zoo::zoo(x[,2],as.Date(x[,1])))

        ECm.st.totp.main<-do.call(merge,ECm.st.totp.zoo)

        results<-list(ECm.st.totp.main,ECm.monthly.data,ECm.monthly.data.nd,ECm.monthly.table,
                      ECm.monthly.main2,ECm.monthly.mwi,ECm.annual,ECm.annual.table,ECm.annual.comp,
                      ECm.annual.peak.data,
                      ECm.annual.peak.table)
        names(results)<-c("Daily.Comp","Monthly.avg","Monthly.avg.nd","Monthly.table",
                          "Monthly.Comp","MWI","Annual","Annual.comp","Annual.Syn","Max.Daily.data",
                          "Max.Daily.table")

        print(cbind(ECm.annual.comp,ECm.monthly.mwi[,13:15]))

        #Preparing Graph
        dwi.table<-dwi(ECm.st.totp.main)


        #Saving Variables
        if(EC==TRUE){
                names(dwi.table)<-paste0(selected.m.network$Station.Name[match(names(dwi.table),
                                                                               selected.m.network$Climate.Identifier)],"-",
                                         selected.m.network$Climate.Identifier[match(names(dwi.table),
                                                                                     selected.m.network$Climate.Identifier)])
        }


        dwi.table<-dwi.table[,ncol(dwi.table):1]

        dwi.table<-dwi.table[as.numeric(rownames(dwi.table))>=matrix.start.year,]

        #Define the output
        assign(output,results,envir = .GlobalEnv)

        stopCluster(cl)

        library(grid)
        library(lattice)
        library(latticeExtra)
        #Colors
        Precipitation.cols <- colorRampPalette(c("white","darkolivegreen3", "lightskyblue", "royalblue3"))
        ColorRamp <- Precipitation.cols
        ncolors<-70

        #Figure
        p<-levelplot(coredata(dwi.table), scales = list(tck = 0, x = list(rot = 90,cex=0.75),
                                                        y = list(rot = 0,cex=0.75)),
                     border="black" ,aspect="fill", margin=F,
                     col.regions = ColorRamp(ncolors),  xlab = "Information Available [years]",
                     ylab = "Stations")

        p$legend$right <-
                list(fun = mergedTrellisLegendGrob(p$legend$right,
                                                   list(fun = textGrob,
                                                        args = list("Days with\ninfo.[days]",
                                                                    rot =0,x=0.3,y=0.2)),
                                                   vertical = T))


        p

}


#' Select stations which have n amount minimum amount of info
#'
#' Select stations which have n amount minimum amount of info
#' @param n     Minimun amount of years
#' @export
ECm.select.min<-function(n=0){ECm.monthly.mwi[ECm.monthly.mwi$Min<n,]$ID}

#' @title Remove Information
#' @description This function removes stations from the database returned from \code{\link[Hydro]{ECm.prepare}} based on the number of months of information. Used to determine useful data.\cr
#' Please Note: \code{\link[Hydro]{ECm.analysis}} should be run before and after this function.
#' @usage
#' ECm.rm(db, x, EC)
#' @param db The database of meteorological information from \code{\link[Hydro]{ECm.prepare}}
#' @param x Criteria for the information to be removed. Reference $MWI from \code{\link[Hydro]{ECm.analysis}}
#' @param EC Specify if the original data was sourced from Environment Canada. If TRUE, function edits information from "selected.m.network" database
#' @examples
#' ## EX 1: Remove stations that have an average months with information less than 10. Using precipitation totals sourced from EC.
#' ECm.rm(db="Total.Precip..mm.db", Total.Precip..mm.syn$MWI$MWI.Avg<10, EC=TRUE)
#' @export
ECm.rm<-function(db="ECm.st.totp",x=0,EC=TRUE){


        ECm.st.totp<-eval(parse(text=db))
        #Prepare Matrix
        y<-matrix(T,length(ECm.st.totp),1)

        y[x]<-F


        #Clean matrixes
        if(EC==TRUE){selected.m.network<<-selected.m.network[y,]}

        assign(db,value = ECm.st.totp[y],envir = .GlobalEnv)

        #ECm.annual.comp<<-ECm.annual.comp[y]
        #ECm.analysis()

}

#'Export the results from analysis
#'@param filename Excel filename to export the results
#'@export
ECm.out<-function(db=Total.Precip..mm.syn,filename="output.xlsx", FA=F,EC=T){
        options(java.parameters = "-Xmx10000m")
        library("xlsx")
        library("zoo")

        #Java Garbage Collector
        jgc <- function()
        {
                .jcall("java/lang/System", method = "gc")
        }


        #Site data.frame
        if(EC==T){
                site.df<-data.frame(longitude=point.location[1],
                                    latitude=point.location[2],
                                    Coast.dist<-coast.dist(longitude = point.location[1],latitude = point.location[2]),
                                    elev<-GE.elev(longitude = point.location[1],latitude = point.location[2]))
                names(site.df)<-c("longitude", "latitude","coast.dist","elev")
                row.names(site.df)<-c("site")

                write.xlsx2(site.df,filename,sheetName = "Site",append=T);jgc()
                write.xlsx2(selected.m.network,filename,sheetName = "Gauge Network");jgc()
        }

        #Saving files
        #Monthly value per station
        for(i in 1:length(db$Monthly.table)){
                write.xlsx2(db$Monthly.table[[i]],filename,
                            sheetName = make.names(names(db$Monthly.table)[i]),append = T)
                jgc()
        }

        write.xlsx2(db$Annual.Syn,filename,sheetName = "MAP",append = T);jgc()
        write.xlsx(db$Annual.comp,filename,sheetName="Annual.TS",append=T);jgc()
        write.xlsx(db$Annual.Syn,filename,sheetName="Annual.syn",append=T);jgc()
        write.xlsx2(db$Monthly.avg,filename,sheetName = "monthly.avg",append = T);jgc()
        write.xlsx2(db$Monthly.avg.nd,filename,sheetName = "monthly.nd.avg",append = T);jgc()
        write.xlsx2(db$Monthly.Comp,filename,sheetName = "monthly.TS",append = T);jgc()
        write.xlsx2(db$MWI,filename,sheetName = "MWI",append = T);jgc()
        write.xlsx2(db$Daily.Comp,filename,sheetName = "daily.TS",append = T);jgc()

        if(FA==T){
                write.xlsx2(freq.table,filename,sheetName = "Freq.Analysis",append = T);jgc()
                write.xlsx2(db$Max.Daily.table,filename,sheetName = "Annual Max",append = T);jgc()
        }



}

#' @title Sub-Daily Preparation
#' @description This function converts the database of sub-daily information to daily information for use in \code{\link[Hydro]{ECm.analysis}}. Parameters requiring sub-daily processing include Wind Speed and Wind Direction.
#' Wind speed and direction are averaged appropriately. Other parameters available sub-daily are averaged using mean.
#' @return a database of daily information
#' @export
ECm.prepare.subdaily<-function(){
        library(parallel)
        library(zoo)

        cl<-makeCluster(detectCores())

        ECm.stations.daily<-parLapply(cl,ECm.stations.subdaily,fun=function(x){

                # x<-ECm.stations.subdaily[[3]]

                sel.col<-grep("Flag",names(x)[6:(ncol(x)-1)],invert = T)+5

                x.df<-data.frame(Date.time=x$Date.Time,
                                 x[,sel.col],stringsAsFactors = F)

                x.zoo<-zoo::read.zoo(x.df)

                x.daily<-hydroTSM::subdaily2daily(x.zoo,FUN = mean,na.rm = T)

                #names(x.daily)

                #Wind speed and wind direction transformation
                dly.ws<-hydroTSM::subdaily2daily(x.daily[,"Wind.Spd..km.h."],FUN=mean,na.rm=T)
                dly.wsmax<-hydroTSM::subdaily2daily(x.daily[,"Wind.Spd..km.h."],FUN=max,na.rm=T)


                dly.wd.sin<-hydroTSM::subdaily2daily(zoo::zoo(sin(x$Wind.Dir..10s.deg.*10/180*pi),x$Date.Time),FUN=mean,na.rm=T)*dly.ws
                dly.wd.cos<-hydroTSM::subdaily2daily(zoo::zoo(cos(x$Wind.Dir..10s.deg.*10/180*pi),x$Date.Time),FUN=mean,na.rm=T)*dly.ws

                dly.wd<-atan2(dly.wd.sin,dly.wd.cos)/pi*180

                dly.wd[dly.wd<0]<-dly.wd[dly.wd<0]+360

                x.daily[,"Wind.Dir..10s.deg."]<-dly.wd/10


                x.df<-data.frame(Date.time=time(x.daily),
                                 zoo::coredata(x.daily),
                                 "Wind.Spd.Max.Hourly..km.h."=dly.wsmax)


        })

        stopCluster(cl)

        ECm.stations.daily<<-ECm.stations.daily
        filename<-paste0("EC Daily Canada Data - ",length(ECm.stations.subdaily)," Stations.RData")
        save(ECm.stations.daily, file=filename,ascii=FALSE, compress=TRUE)

}



#'Return a list of dataframe with the update undercatchment values for a meteorological parameter (EC) - Monthly
#'@param var Meteorological parameter for undercatchment. Value between: Precipitation (Precipitation), Snow (Snow), Rain (Rain), Wind Speed (Wind), Pressure (Pressure), Sea Pressure (Sea.Pressure), Min Temperature (Min.Temp), Mean Temperature (Mean.Temp) and Maximum Temperature (Max.Temp)
#'@export

UC<-function(var="Precipitation"){

        library(RCurl)
        library(reshape2)
        library(parallel)

        Precipitation.l<-"ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/Adj_monthly_total_prec.zip"
        Snow.l<-"ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/Adj_monthly_snow.zip"
        Rain.l<-"ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/Adj_monthly_rain.zip"
        Wind.l<-"ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/Homog_monthly_wind_speed.zip"
        Pressure.l<-"ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/Homog_monthly_station_pressure.zip"
        Sea.Pressure.l<-"ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/Homog_monthly_sea_level_pressure.zip"
        Min.Temp.l<-"ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/Homog_monthly_min_temp.zip"
        Mean.Temp.l<-"ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/Homog_monthly_mean_temp.zip"
        Max.Temp.L<-"ftp://ccrp.tor.ec.gc.ca/pub/AHCCD/Homog_monthly_max_temp.zip"

        link<-NA

        if(var=="Precipitation"){link<-Precipitation.l}
        if(var=="Snow"){link<-Snow.l}
        if(var=="Rain"){link<-Rain.l}
        if(var=="Pressure"){link<-Pressure.l}
        if(var=="Sea.Pressure"){link<-Sea.Pressure.l}
        if(var=="Min.Temp"){link<-Min.Temp.l}
        if(var=="Mean.Temp"){link<-Mean.Temp.l}
        if(var=="Max.Temp"){link<-Max.Temp.l}

        if(is.na(link)){print("Selected from: Precipitation, Snow, Rain, Min.Temp, Mean.Temp, Max.Temp, Pressure, Sea.Pressure")
        }
        else{

                temp <- tempfile()

                download.file(link,temp)

                dir.path<-"./data/AHCCD"
                unzip(temp,exdir=dir.path)
                #Define excel files at the excel path
                AHCCD.files<-list.files(path=dir.path)

                cl<-makeCluster(detectCores())
                clusterExport(cl,"dir.path",envir = environment())

                Adj.values<-parLapply(cl,AHCCD.files,fun=function(x){
                        #x<-AHCCD.files[[1]]
                        header<-read.csv(paste0(dir.path,"/",x), header=FALSE, row.names=1, stringsAsFactors=FALSE,nrows=1)
                        print(as.character(header)[1])

                        table <- read.csv(paste0(dir.path,"/",x), header=FALSE, row.names=1, stringsAsFactors=FALSE,skip=2)

                        table<-table[-c(1,2),as.numeric(sapply(month.abb,FUN=function(x){grep(x,table[1,])}))]
                        #table<-cbind(rownames(table),table)
                        names(table)<-c(month.abb)

                        table.TS<-reshape2::melt(t(table))

                        table.TS<-data.frame(Date=as.POSIXlt(paste0("1-",table.TS$Var1,"-",table.TS$Var2),format = "%e-%b-%Y",tz = "UTC"),
                                             Value=table.TS$value,stringsAsFactors = F)
                        table.TS$Value<-as.numeric(as.character(table.TS$Value))

                        table.TS$Value[table.TS$Value==-9999.9]<-NA

                        table.TS
                })

                stopCluster(cl)
                names(Adj.values)<-substr(AHCCD.files,3,9)

                unlink("./data/AHCCD",recursive = T)

                Adj.values
        }
}

#'Return a list of dataframe with the update undercatchment values for a meteorological parameter (EC) - Daily
#'@param var Meteorological parameter for undercatchment. Value between: Precipitation (Precipitation), Snow (Snow), Rain (Rain), Min Temperature (Min.Temp), Mean Temperature (Mean.Temp) and Maximum Temperature (Max.Temp)
#'@export

UC.dly<-function(var="Precipitation"){

        library(RCurl)
        library(reshape2)
        library(parallel)

        Precipitation.l<-"ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/Adj_Daily_TotalP_v2016.zip"
        Snow.l<-"ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/Adj_Daily_Snow_v2016.zip"
        Rain.l<-"ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/Adj_Daily_Rain_v2016.zip"
        Min.Temp.l<-"ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/Homog_daily_min_temp_v2016.zip"
        Mean.Temp.l<-"ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/Homog_daily_mean_temp_v2016.zip"
        Max.Temp.L<-"ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/Homog_daily_max_temp_v2016.zip"

        link<-NA

        if(var=="Precipitation"){link<-Precipitation.l}
        if(var=="Snow"){link<-Snow.l}
        if(var=="Rain"){link<-Rain.l}
        if(var=="Min.Temp"){link<-Min.Temp.l}
        if(var=="Mean.Temp"){link<-Mean.Temp.l}
        if(var=="Max.Temp"){link<-Max.Temp.l}

        print("Version 2016 - ftp://ccrp.tor.ec.gc.ca/pub/EC_data/AHCCD_daily/")
        if(is.na(link)){print("Selected from: Precipitation, Snow, Rain, Min.Temp, Mean.Temp, Max.Temp")
        }
        else{

                temp <- tempfile()

                download.file(link,temp)

                dir.path<-"./data/AHCCD"
                unzip(temp,exdir=dir.path)
                #Define excel files at the excel path
                AHCCD.files<-list.files(path=dir.path)

                cl<-makeCluster(detectCores())
                clusterExport(cl,"dir.path",envir = environment())

                Adj.values<-parLapply(cl,AHCCD.files,fun=function(x){


                        #x<-AHCCD.files[[1]]

                        # header<-read.csv(paste0(dir.path,"/",x), header=FALSE, row.names=1, stringsAsFactors=FALSE,nrows=1)
                        #
                        # header2<-read.csv(paste0(dir.path,"/",x), header=FALSE,  stringsAsFactors=FALSE,
                        #                   skip=6,nrows=1,sep=" ")

                        table<-readr::read_fwf(file = paste0(dir.path,"/",x),skip = 1,
                                               readr::fwf_widths(c(4,4,rep(c(8,1),31))))

                        table<-table[,-seq(4,64,2)]

                        table<-data.frame(table)

                        table.zf<-apply(table,1,FUN=function(x){
                                # x<-table[1,]

                                dim<-lubridate::days_in_month(as.Date(paste0(x[1],"-",x[2],"-01")))

                                zoo::zoo(as.numeric(x[(1+2):(dim+2)]),
                                         seq.Date(from = as.Date(paste0(x[1],"-",x[2],"-01")),length.out = dim,by="day"))

                        })

                        table.z<-do.call(rbind,table.zf)

                        table.z[table.z==-9999.99]<-NA

                        table.z

                })

                stopCluster(cl)
                names(Adj.values)<-substr(AHCCD.files,3,9)

                unlink("./data/AHCCD",recursive = T)

                Adj.values
        }
}


#'Perform a transformation from daily to montly wind speed and wind direction
#'
#'Perform a transformation from daily to montly wind speed and wind direction
#'@param ws.ts Wind Speed Time series in zoo format, wd.deg.ts Wind direction time series in degrees in zoo format, max.miss Maximum of missing days to be considered a complete month.
#'@export
wind.daily2monthly<-function(ws.ts,wd.deg.ts,max.miss=5){


        #Raw mean
        raw.ws.mon<-hydroTSM::daily2monthly(ws.ts,FUN = mean,na.rm = T)
        raw.wd.mon<-hydroTSM::daily2monthly(wd.deg.ts,FUN = mean,na.rm = T)

        #Corrected
        mon.wd.sin<-hydroTSM::daily2monthly(sin(wd.deg.ts/180*pi),FUN=mean,na.rm=T)*raw.ws.mon

        mon.wd.cos<-hydroTSM::daily2monthly(cos(wd.deg.ts/180*pi),FUN=mean,na.rm=T)*raw.ws.mon

        mon.wd<-atan2(mon.wd.sin,mon.wd.cos)/pi*180

        mon.wd[mon.wd<0]<-mon.wd[mon.wd<0]+360

        #Corrected Wd
        raw.wd.mon<-mon.wd

        res.zoo<-merge(raw.ws.mon,raw.wd.mon)

        names(res.zoo)<-c("Wind.Speed","Wind.Direction")

        res.zoo

        #Filtering process
        library(lubridate)
        library(Hmisc)
        max.avail.ws<-aggregate(ws.ts,by=list(as.Date(paste0(year(ws.ts),"-",month(ws.ts),"-1"))),
                                FUN=function(x){sum(!is.na(x),na.rm=T)})

        max.avail.wd<-aggregate(wd.deg.ts,by=list(as.Date(paste0(year(wd.deg.ts),"-",month(wd.deg.ts),"-1"))),
                                FUN=function(x){sum(!is.na(x),na.rm=T)})



        date.avail<-unique(as.Date(strptime(paste0(year(time(ws.ts)),"-",month(time(ws.ts)),"-1"),"%Y-%m-%d")))

        data.avail.c<-pmin(max.avail.wd,max.avail.ws)+max.miss>=monthDays(date.avail)

        res.zoo[!as.logical(data.avail.c),]<-NA

        res.zoo
}

#'Transform degrees in compass direction
#'
#'Transform degrees in compass direction
#'@param num Degree value of wind speed to be transformed in compass direction
#'@export
degtocompass<-function(num){

        arr<-c("N","NNE","NE","ENE","E","ESE", "SE", "SSE","S","SSW","SW","WSW","W","WNW","NW","NNW","N")
        arr[ceiling((num+11.25)/22.5)]
}



#' Index table for EC Meteorology
#' @author Victor Munoz
#' @references EC
#' @format Index table for EC Meteorology
#' @name station.db
#' @usage data(station.db)
NULL
