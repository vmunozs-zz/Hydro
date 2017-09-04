#' Find the stations from NOAA
#'
#' Find the stations from NOAA
#'
#' @export
NOAA.find<-function(longitude=12.096522, latitude=-4.592616, radius_deg=5 ){
        library("rnoaa")
        #library("EcoHydRology", lib.loc="~/R/win-library/3.1")
        library("lubridate")
        library("zoo")
        library("RCurl") #Network interfase
        library("geosphere") #Distance from coordinates

        radius<-radius_deg*110

        #options(noaakey="zccPPqgbhBjNoKfRCSRVULogLUIMUMdA") #vmunoz@srk.com
        options(noaakey="PacdvUWVYvHOPwXrHRILLOJpvDnORxEB") #vmunozs@gmail.com
        #options(noaakey="jXRCZvRLvthgwDGqgtVSVqoqeZCwkIcP") #marcela.poncem@gmail.com
        #options(noaakey = "NjuEQnGAERfxLrZNIiHqMUJsSeqiPuRR") #third party

        point.location<<-c(longitude,latitude)

        info<-ncdc_stations(datasetid='GHCND',
                            extent=c(point.location[2]-radius_deg,point.location[1]-radius_deg,point.location[2]+radius_deg,point.location[1]+radius_deg),
                            limit=1000)

        selected.NOAA.GHCND<-info$data
        #_________________________________
        names(point.location)<-(c("Longitude","Latitude"))
        #Obtain coordiantes
        station.coordinates<-cbind(selected.NOAA.GHCND$longitude,selected.NOAA.GHCND$latitude)

        #Obtain distance from interesting point to Gauge stations. Info is passed to Km and rounded.
        station.distance<-round(distVincentySphere(point.location,station.coordinates)/1000,2)

        #Including distance in the network file
        selected.NOAA.GHCND<-cbind(selected.NOAA.GHCND,station.distance)

        #Sorted unregulated stations
        selected.NOAA.GHCND<-selected.NOAA.GHCND[order(station.distance),]

        #Remove station with less than 5 years
        selected.NOAA.GHCND<-selected.NOAA.GHCND[year(selected.NOAA.GHCND$maxdate)-
                                                         year(selected.NOAA.GHCND$mindate)>=5,]

        selected.NOAA.GHCND<<-selected.NOAA.GHCND


        #Prepare DB for GSOD
        GSOD.db<-getURL(url = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
        GSOD.db<-read.csv(text=GSOD.db,encoding="UTF-8",header = TRUE)
        complete.GSOD<-complete.cases(GSOD.db$LAT,GSOD.db$LON)
        GSOD.db<-GSOD.db[complete.GSOD,]

        #Cleaning GSOD.db for correct coordinates
        GSOD.db<-GSOD.db[!(GSOD.db$LON>180|GSOD.db$LON< -180|GSOD.db$LAT>90|GSOD.db$LAT< -90),]

        #Obtain coordiantes
        station.coordinates<-cbind(GSOD.db$LON,GSOD.db$LAT)

        #Obtain distance from interesting point to Gauge stations. Info is passed to Km and rounded.
        station.distance<-round(distVincentySphere(point.location,station.coordinates)/1000,2)

        #Including distance in the network file
        selected.NOAA.GSOD<-cbind(GSOD.db,station.distance)

        #Sorted unregulated stations
        selected.NOAA.GSOD<-selected.NOAA.GSOD[order(station.distance),]

        #Sort by radius
        selected.NOAA.GSOD<-selected.NOAA.GSOD[selected.NOAA.GSOD$station.distance<radius,]

        #Remove station with less than 5 years
        selected.NOAA.GSOD<-selected.NOAA.GSOD[year(strptime(selected.NOAA.GSOD$END,format = "%Y%m%d"))-year(strptime(selected.NOAA.GSOD$BEGIN,format = "%Y%m%d"))>=5,]

        selected.NOAA.GSOD<<-selected.NOAA.GSOD

        message ("DB Global Summary of the Day (GSOD)             : ",nrow(selected.NOAA.GSOD)," Stations")
        message ("DB Global Historical Climatology Network (GHCND): ",nrow(selected.NOAA.GHCND)," Stations")

}


#' Capture the information from NOAA GHCND
#'
#' Capture the information from NOAA GHCND
#'
#' @export
NOAA.capture.GHCND<-function(noaakey="zccPPqgbhBjNoKfRCSRVULogLUIMUMdA"){

            #options(noaakey="zccPPqgbhBjNoKfRCSRVULogLUIMUMdA") #vmunoz@srk.com
            #options(noaakey="PacdvUWVYvHOPwXrHRILLOJpvDnORxEB") #vmunozs@gmail.com
            #options(noaakey="jXRCZvRLvthgwDGqgtVSVqoqeZCwkIcP") #marcela.poncem@gmail.com
            #options(noaakey = "NjuEQnGAERfxLrZNIiHqMUJsSeqiPuRR") #third party

            library(lubridate)
            library(parallel)
            library(rnoaa)
            cl<-makeCluster(detectCores())

            NOAA.stations<-NULL
            for(i in 1: nrow(selected.NOAA.GHCND)){
                        #i<-2
                        print(paste("Station ",i,"/",nrow(selected.NOAA.GHCND),":",selected.NOAA.GHCND$name[i],sep=""))
                        a<-NULL
                        start.station<-year(selected.NOAA.GHCND$mindate[i])
                        end.station<-year(selected.NOAA.GHCND$maxdate[i])
                        total.years<-end.station-start.station+1
                        total.years.list<-1:total.years



                        clusterExport(cl,varlist = c("total.years",
                                                     "start.station","end.station",
                                                     "selected.NOAA.GHCND","i","a","noaakey"),envir = environment())

                        results<-parLapply(cl,total.years.list,fun=function(n){
                                    #i<-1;n<-1
                                    options(noaakey=noaakey)

                                    print(paste(n,"/",total.years,sep=""))

                                    #divide the year in two
                                    start1<-paste(n+start.station-1,"-01-01",sep="")
                                    end1<-paste(n+start.station-1,"-06-30",sep="")

                                    start2<-paste(n+start.station-1,"-07-01",sep="")
                                    end2<-paste(n+start.station-1,"-12-31",sep="")

                                    #obtain two pieces
                                    a1<-rnoaa::ncdc(datasetid='GHCND',stationid=selected.NOAA.GHCND$id[i],
                                                    startdate=start1, enddate=end1,limit=1000)

                                    a2<-rnoaa::ncdc(datasetid='GHCND',stationid=selected.NOAA.GHCND$id[i],
                                                    startdate=start2, enddate=end2,limit=1000)

                                    #Combine
                                    if(is.null(nrow(a1$data))){comb_year<-a2}
                                    if(is.null(nrow(a2$data))){comb_year<-a1}
                                    if(!is.null(nrow(a1$data))&!is.null(nrow(a2$data))){comb_year<-rnoaa::ncdc_combine(a1,a2)}

                                    #Stored info for one year
                                    #a[n]<-list(comb_year)
                                    comb_year

                        })

                        a<-results

                        #Select the years with info
                        sel.a<-lapply(a,FUN=function(x){!is.null(nrow(x$data))})

                        #Remove the info which is not used
                        a<-a[as.logical(sel.a)]

                        #combine the information
                        b<-do.call(ncdc_combine,a)

                        #preparing a zoo

                        if(is.null(nrow(b$data))){
                                    #Exception if there is no information
                                    station.info<-NULL
                        } else{
                                    b<-b$data
                                    tmax<-b[b$datatype=="TMAX",]
                                    tmin<-b[b$datatype=="TMIN",]
                                    pp<-b[b$datatype=="PRCP",]

                                    pp.df<-data.frame(as.Date(pp$date,"%Y-%m-%d"),
                                                      as.character(pp$station),
                                                      as.numeric(pp$value)/10,
                                                      as.character(pp$fl_so))
                                    names(pp.df)<-c("date","station","pp","pp_F")

                                    tmin.df<-data.frame(as.Date(tmin$date,"%Y-%m-%d"),
                                                        as.character(tmin$station),
                                                        as.numeric(tmin$value)/10,
                                                        as.character(tmin$fl_so))
                                    names(tmin.df)<-c("date","station","tmin","tmin_F")

                                    tmax.df<-data.frame(as.Date(tmax$date,"%Y-%m-%d"),
                                                        as.character(tmax$station),
                                                        as.numeric(tmax$value)/10,
                                                        as.character(tmax$fl_so))
                                    names(tmax.df)<-c("date","station","tmax","tmax_F")

                                    temp.df<-merge(tmax.df,tmin.df,all = TRUE)
                                    station.info<-merge(temp.df,pp.df,all=TRUE)

                                    #Settting start and end of TS

                                    start1<-as.Date(paste(year(b$date[length(b)]),"-01-01",sep=""))
                                    end1<-as.Date(paste(year(b$date[nrow(b)]),"-12-31",sep=""))

                                    d<-data.frame(seq(from = start1,to = end1,by = 1),
                                                  rep(b$station[1],times = as.numeric (end1-start1+1)),
                                                  stringsAsFactors = FALSE)

                                    names(d)<-c("date", "station")

                                    #compilation with corrected TS
                                    station.info<-merge(d,station.info,all=TRUE)

                        }

                        NOAA.stations[i]<-list(station.info)

            }

            stopCluster(cl)

            names(NOAA.stations)<-selected.NOAA.GHCND$id[1:length(NOAA.stations)]
            NOAA.stations.GHCND<<-NOAA.stations

            save(list=c("selected.NOAA.GHCND","NOAA.stations.GHCND"),
                 file=paste0("NOAA GHCND ",length(NOAA.stations.GHCND)," Stations.RData"),ascii=FALSE, compress=TRUE)

}


#' Capture the information from NOAA GSOD
#'
#' Capture the information from NOAA GSOD
#'
#' @export
NOAA.capture.GSOD<-function(){

        library("RCurl") #Network interfase


        NOAA.stations.GSOD<-lapply(1: nrow(selected.NOAA.GSOD),FUN=function(j){

                print(paste0(j,"/",nrow(selected.NOAA.GSOD)))

                station<-selected.NOAA.GSOD[j,]
                station.start<-floor(station$BEGIN/10000)
                station.end<-floor(station$END/10000)

                range.years<-station.start:station.end

                #Function for url
                dlurl<-function(station.year){paste("http://www1.ncdc.noaa.gov/pub/data/gsod/",
                                                    station.year,"/",station$USAF,"-",station$WBAN,"-",station.year,".op.gz"
                                                    ,sep="")}

                #List of available years
                avail.years<-sapply(range.years,FUN=function(station.year)url.exists(dlurl(station.year)))

                #reduce the rang of years based on available
                range.years<-range.years[avail.years]

                cl<-makeCluster(detectCores())

                clusterExport(cl,varlist = c("dlurl","station"),envir = environment())


                station.parts<-parLapply(cl,range.years,fun=function(station.year){

                        #print(paste(station.year,"/",nrow(selected.NOAA.GSOD)))

                        #unlink("temp.gz")

                        tmp.file<-tempfile()

                        #downloading file
                        temp<-download.file(url = dlurl(station.year),destfile = tmp.file,quiet = TRUE)
                        #read file
                        annual.file<-read.table(file=tmp.file,skip=1,
                                                head=FALSE,
                                                na.strings=c("9999.9","999.9","99.99"),
                                                stringsAsFactors = FALSE)
                        #defining headings
                        names(annual.file)<-c("STN---", "WBAN","YEARMODA","TEMP","TEMP_F","DEWP", "DEWP_F",
                                              "SLP","SLP_F","STP","STP_F","VISIB", "VISIB_F","WDSP",
                                              "MXSPD","F_MXSPD", "GUST","MAX","MIN","PRCP","SNDP","FRSHTT")

                        unlink(tmp.file)

                        annual.file

                })

                stopCluster(cl)

                b<-do.call(rbind,station.parts)


                #Preparing info in correct format
                if (!is.null(b)){

                        #Date format
                        YEARMODA.df<-data.frame(as.Date(as.character(b$YEARMODA),format = "%Y%m%d"))
                        names(YEARMODA.df)<-c("YEARMODA")

                        #Precipitation format
                        PRCP.df<-data.frame(round(as.numeric(substr(b$PRCP,1,4))*25.4,1), #Values for Pp from inch to millimeters
                                            as.character(substr(b$PRCP,5,5)), #Flag for Pp
                                            stringsAsFactors = FALSE)
                        names(PRCP.df)<-c("PRCP","PRCP_F")

                        #Temperature & Dew Point Format
                        TEMP.df<-data.frame(round(as.numeric((b$TEMP-32)*5/9),1),
                                            as.character(b$TEMP_F),
                                            round(as.numeric((b$DEWP-32)*5/9),1),
                                            as.character(b$DEWP_F))
                        names(TEMP.df)<-c("TEMP","TEMP_F","DEWP","DEWP_F")


                        #arrange of data frame per station
                        d<-data.frame(
                                b[,1:2],
                                YEARMODA.df,
                                TEMP.df,
                                b[,8:19],
                                PRCP.df,
                                b[,21:22],stringsAsFactors = FALSE)

                        #Defining starting and ending point
                        start1<-as.Date(paste(year(d$YEARMODA[[1]]),"-01-01",sep=""))
                        end1<-as.Date(paste(year(d$YEARMODA[nrow(d)]),"-12-31",sep=""))

                        #Dummy Data frame to extend and correct starting and end
                        f<-data.frame(rep(d$STN...[1],times = as.numeric (end1-start1+1)),
                                      rep(d$WBAN[1],times = as.numeric (end1-start1+1)),
                                      seq(from = start1,to = end1,by = 1),
                                      stringsAsFactors = FALSE)

                        names(f)<-c("STN...", "WBAN","YEARMODA")

                        #Corrected extension
                        b<-merge(f,d,all=TRUE)
                }
                #listing
                b

        })

        NOAA.stations.GSOD.avail<-lapply(NOAA.stations.GSOD,FUN=function(x){
                #x<-NOAA.stations.GSOD[[8]]
                if(!is.null(x$STN[1])){TRUE}else{FALSE}
        })

        NOAA.stations.GSOD.avail<-as.vector(unlist(NOAA.stations.GSOD.avail))

        #Saving Info
        NOAA.stations.GSOD<-NOAA.stations.GSOD[NOAA.stations.GSOD.avail]

        #Define names
        GSOD.names<-sapply(NOAA.stations.GSOD,FUN=function(x){
                paste0(x$STN[[1]],x$WBAN[[1]])
        })

        GSOD.names<-make.names(paste("GSOD:",GSOD.names,sep=""))
        names(NOAA.stations.GSOD)<-GSOD.names

        NOAA.stations.GSOD<<-NOAA.stations.GSOD

        save(list=c("selected.NOAA.GSOD","NOAA.stations.GSOD"),
             file=paste0("NOAA GSOD ",length(NOAA.stations.GSOD)," Stations.RData"),ascii=FALSE, compress=TRUE)

}


#' Prepare the information from NOAA
#'
#' Prepare the information from NOAA.
#' @param par "Pp" for Precipitation, "Tmean" for Avg. Temp, "Tmax" for Max. Temperature, "Tmin" for Min. Temperature
#'
#' @export
NOAA.prepare<-function(par="Pp",output= "ECm.st.totp"){

        library(lubridate)
        library(zoo)
        library(stringr)

        #Case of parameter Precipitation
        if(par=="Pp"){
                #compiling precipitation info from two databases
                comp.GHCND<-lapply(NOAA.stations.GHCND,FUN = function(i){
                        #i<-NOAA.stations.GHCND[[4]]
                        j<-data.frame(date=i$date,
                                      station=i$station,
                                      pp=i$pp,
                                      stringsAsFactors = FALSE)
                        j
                })
                comp.GSOD<-lapply(NOAA.stations.GSOD,FUN = function(i){
                        #j<-NOAA.stations.GSOD[[72]]
                        if(is.null(i)){
                                NULL
                        } else{
                                j<-data.frame(date=i$YEARMODA,
                                              station=paste("GSOD:",i$STN...,i$WBAN,sep=""),
                                              pp=i$PRCP,
                                              stringsAsFactors = FALSE)
                                j}
                })}

        #Case of parameter Tmean
        if(par=="Tmean"){
                comp.GHCND <- lapply(NOAA.stations.GHCND, FUN = function(i) {
                        #i<-NOAA.stations.GHCND[[1]]
                        j <- data.frame(date = i$date, station = i$station,
                                        tmean=apply(data.frame(i$tmax,i$tmin),1,FUN=function(x){mean(x,na.rm=F)}),
                                        stringsAsFactors = FALSE)
                        j
                })
                comp.GSOD <- lapply(NOAA.stations.GSOD, FUN = function(i) {
                        if (is.null(i)) {
                                NULL
                        }
                        else {
                                j <- data.frame(date = i$YEARMODA, station = paste("GSOD:",
                                                                                   i$STN...,i$WBAN, sep = ""),
                                                tmean = i$TEMP, stringsAsFactors = FALSE)
                                j
                        }
                })}

        #Case of parameter Tmax
        if(par=="Tmax"){
                comp.GHCND <- lapply(NOAA.stations.GHCND, FUN = function(i) {
                        #i<-NOAA.stations.GHCND[[1]]
                        j <- data.frame(date = i$date, station = i$station,
                                        tmax=i$tmax,
                                        stringsAsFactors = FALSE)
                        j
                })
                comp.GSOD <- lapply(NOAA.stations.GSOD, FUN = function(i) {
                        NULL
                })

        }

        #Case of parameter Tmin
        if(par=="Tmin"){
                comp.GHCND <- lapply(NOAA.stations.GHCND, FUN = function(i) {
                        #i<-NOAA.stations.GHCND[[1]]
                        j <- data.frame(date = i$date, station = i$station,
                                        tmin=i$tmin,
                                        stringsAsFactors = FALSE)
                        j
                })
                comp.GSOD <- lapply(NOAA.stations.GSOD, FUN = function(i) {
                        NULL
                })}
        # names(comp.GSOD)<-paste("GSOD:",names(comp.GSOD),sep="")

        names(comp.GSOD)<-str_replace(names(comp.GSOD),"\\.",":")


        NOAA.stations<-modifyList(comp.GHCND,comp.GSOD)

        #saving compiled DB
        NOAA.stations<<-NOAA.stations

        #compiling DB index1
        selected.NOAA.GSOD.corrected<-data.frame(paste("GSOD:",selected.NOAA.GSOD$USAF,selected.NOAA.GSOD$WBAN,sep=""),
                                                 as.character(selected.NOAA.GSOD$STATION.NAME),
                                                 selected.NOAA.GSOD$LAT,
                                                 selected.NOAA.GSOD$LON,
                                                 selected.NOAA.GSOD$ELEV.M.,
                                                 as.Date(strptime(selected.NOAA.GSOD$BEGIN,"%Y%m%d")),
                                                 as.Date(strptime(selected.NOAA.GSOD$END,"%Y%m%d")),
                                                 selected.NOAA.GSOD$station.distance,
                                                 stringsAsFactors = FALSE)
        names(selected.NOAA.GSOD.corrected)<-c("ID","Station","Latitude","Longitude","Elevation",
                                               "Start","End","Distance")
        #compiling DB index2
        selected.NOAA.GHCND.corrected<-data.frame(selected.NOAA.GHCND$id,
                                                  selected.NOAA.GHCND$name,
                                                  selected.NOAA.GHCND$latitude,
                                                  selected.NOAA.GHCND$longitude,
                                                  selected.NOAA.GHCND$elevation,
                                                  as.Date(selected.NOAA.GHCND$mindate),
                                                  as.Date(selected.NOAA.GHCND$maxdate),
                                                  selected.NOAA.GHCND$station.distance,
                                                  stringsAsFactors = FALSE)
        names(selected.NOAA.GHCND.corrected)<-c("ID","Station","Latitude","Longitude","Elevation",
                                                "Start","End","Distance")
        #combining DB
        selected.NOAA.network<-rbind(selected.NOAA.GHCND.corrected,selected.NOAA.GSOD.corrected)

        #Sorted unregulated stations
        selected.NOAA.network<-selected.NOAA.network[order(selected.NOAA.network$Distance),]

        selected.NOAA.network<<-selected.NOAA.network


        #sort the info based on ECm.find
        ECm.stations.red<-NOAA.stations[selected.NOAA.network$ID]

        #just info with the useful data frame
        Resultant.DF<-lapply(ECm.stations.red,function(i){
                #i<-ECm.stations.red[[27]]
                if(is.null(i)|length(i)==0){
                        NULL
                }else{

                        i<-data.frame(i[,c(1,3)])
                        names(i)<-c("Date",par)
                        i}


        })

        #removing null element
        y<-lapply(Resultant.DF, is.null)
        y<-!unlist(y)
        Resultant.DF<-Resultant.DF[y]

        # names(Resultant.DF)<-make.names(names(Resultant.DF),unique = T)

        selected.NOAA.network<-selected.NOAA.network[y,]

        assign(output,Resultant.DF,envir = .GlobalEnv)

        selected.NOAA.network<<-selected.NOAA.network

}




#' Analize the information from NOAA
#'
#' Analize the information from NOAA
#'
#' @export
NOAA.analysis<-function(monthly.zero=NA){
        library("hydroTSM")
        library("matrixStats")

        ECm.monthly.ts<-NULL
        ECm.annual.comp<-NULL
        ECm.monthly.table<-NULL
        ECm.monthly<-NULL
        ECm.monthly.mwi<-NULL
        names(ECm.st.totp)<-make.names(names(ECm.st.totp),unique = T)
        selected.NOAA.network$ID<-make.names(names(ECm.st.totp),unique = T)

        for(i in 1:length(ECm.st.totp)){
                # i<-4
                 #i<-3
                x<-zoo(ECm.st.totp[[i]]$Precipitation,ECm.st.totp[[i]]$Date)

                #Removing months with less than 5 days missing (implicit in the expr.)
                y<-daily2monthly.V2(x = x,FUN = sum)

                #monthly zero = user definition
                y[y==0]<-monthly.zero
                #complete missing values
                ECm.monthly.ts[i]<-list(y)

                #Monthly Table
                ECm.monthly.table[i]<-list(zoo2matrix(y))

                #Monthly Average Table
                ECm.monthly[i]<-list(colMeans(ECm.monthly.table[[i]], na.rm=TRUE))

                #Amount of info
                ECm.monthly.mwi[i]<-list(colCounts(!is.na(ECm.monthly.table[[i]])))
        }

        names(ECm.monthly.table)<-make.names(names(ECm.st.totp),unique = T)
        names(ECm.monthly)<-make.names(names(ECm.st.totp),unique = T)
        names(ECm.monthly.mwi)<-make.names(names(ECm.st.totp),unique = T)
        names(ECm.monthly.ts)<-make.names(names(ECm.st.totp),unique = T)

        #Monthly Information Compilation
        ECm.monthly.data<-matrix(data=unlist(ECm.monthly),byrow = TRUE, nrow=(length(ECm.monthly)),ncol=12)
        rownames(ECm.monthly.data)<-unlist(names(ECm.st.totp))
        colnames(ECm.monthly.data)<-month.abb
        ECm.monthly.data<-as.data.frame(ECm.monthly.data)

        #Monthly Information Compilation for MWI
        ECm.monthly.mwi<-matrix(data=unlist(ECm.monthly.mwi),byrow = TRUE, nrow=(length(ECm.monthly.mwi)),ncol=12)
        rownames(ECm.monthly.mwi)<-c(make.names(names(ECm.st.totp),unique = T))

        ECm.monthly.mwi<-as.data.frame(ECm.monthly.mwi)

        #Incliding min in mwi table
        monthly.min.mwi<-apply(ECm.monthly.mwi[c(1:12)],1,function(x){min(x)})

        #Incliding max in mwi table
        monthly.max.mwi<-apply(ECm.monthly.mwi[c(1:12)],1,function(x){max(x)})

        #Including Avg
        monthly.avg.mwi<-round(rowMeans(ECm.monthly.mwi),0)

        #Include id
        monthly.id.mwi<- 1:length(ECm.st.totp)

        #cbind min,max,avg and ID
        ECm.monthly.mwi<-cbind(ECm.monthly.mwi,monthly.min.mwi,monthly.max.mwi,monthly.avg.mwi,monthly.id.mwi)


        #Including names
        colnames(ECm.monthly.mwi)<-c(month.abb,"Min", "Max","Avg","ID")

        #Mean Annual Pp
        ECm.annual.comp<-as.data.frame(rowSums(ECm.monthly.data))

        #Include Elevation & Distance
        ECm.annual.comp<-cbind(ECm.annual.comp,selected.NOAA.network$Elevation,selected.NOAA.network$Distance)

        ECm.annual.comp<-cbind(selected.NOAA.network[c("Station","Longitude","Latitude")],ECm.annual.comp)
        names(ECm.annual.comp)<-c("Name","Longitude","Latitude","MAP","Elevation","Distance")

        #Include non dimensional precipitation
        ECm.monthly.data.nd<-round(ECm.monthly.data/rowSums(ECm.monthly.data),3)
        ECm.monthly.data.nd<-cbind(selected.NOAA.network$Station,ECm.monthly.data.nd)
        names(ECm.monthly.data.nd)<-c("Station",month.abb)

        # Daily Max Analysis
        ECm.annual.peak.data<-lapply(ECm.st.totp,function(i){
                #i<-ECm.st.totp[[1]]

                x<-zoo(i[,2],i[,1])
                y<-daily2monthly.V2(x = x,FUN = sum)
                z<-zoo2matrix(y)
                years<-rownames(z)

                Annual.Peak<-NULL
                for (n in 1:length(years)){
                        #n<-20
                        start<-match(as.Date(paste(years[n],"-01-01",sep="")),i[,1])
                        end<-match(as.Date(paste(years[n],"-12-31",sep="")),i[,1])

                        if(is.na(start)|is.na(end)){
                                Annual.Peak[n]<-NA
                                next
                        }
                        if(sum(!is.na(i[start:end,2]))>360) {
                                Annual.Peak[n]<-max(i[start:end,2],na.rm=TRUE)
                        }else{
                                Annual.Peak[n]<-NA
                        }
                }

                table<-as.data.frame(cbind(years,Annual.Peak),stringsAsFactors = FALSE)
                table[,1]<- unique(as.numeric(as.character(table[,1])))
                table[,2]<- as.numeric(as.character(table[,2]))
                names(table)<-c("Years","Annual Peak")
                table
        })
        ##Daily Max Table
        ECm.annual.peak.table<-ECm.annual.peak.data[[1]]
        names(ECm.annual.peak.table)<-c("Years",names(ECm.annual.peak.data)[1])
        n<-length(ECm.annual.peak.data)
        for(i in 2:n){
                x<-ECm.annual.peak.data[[i]]
                names(x)<-c("Years",names(ECm.annual.peak.data)[i])
                ECm.annual.peak.table<-merge(ECm.annual.peak.table,x,"Years",all=TRUE)
        }


        #compile daily as a zoo
        ECm.st.totp.zoo<-lapply(ECm.st.totp,function(x){
                zoo(x[,2],x[,1])

        })

        #combine in just one database
        ECm.st.totp.comp<-do.call(merge,ECm.st.totp.zoo)

        #combine in just one database
        monthly.ts.comp<-do.call(merge,ECm.monthly.ts)


        #packing the file
        NOAA.stations.syn<-list(ECm.st.totp.comp, ECm.st.totp,ECm.monthly.data, ECm.monthly.table,
                                ECm.monthly.ts, monthly.ts.comp,ECm.monthly.data.nd,ECm.monthly.mwi,
                                ECm.annual.comp,ECm.annual.peak.data, ECm.annual.peak.table )
        names(NOAA.stations.syn)<-c("daily.ts.comp","daily.ts","monthly.avg","monthly.table",
                                    "monthly.ts", "monthly.ts.comp","monthly.avg.nd","mwi",
                                    "annual.avg.table","annual.peak.table","annual.peak.table.comp")

        NOAA.stations.syn<<-NOAA.stations.syn

        #Preparing Graph
        matrixplot(dwi(ECm.st.totp.comp), ColorRamp = "Days")

}

ECm.save<-function(){
        #setwd("//VAN-SVR0/Projects/01_SITES/Grassy Mountain/!020_Site_Wide_Data/Hydrology/Precipitation")
        #save(ECm.stations, file="EC Meteorological Stations Daily Data.RData",ascii=FALSE, compress=TRUE)
        #save(selected.m.network, file="Selected Meteorological Stations.RData",ascii=FALSE, compress=TRUE)
        #save(station.db, file="EC Meteorological DB.RData",ascii=FALSE, compress=TRUE)
        write.csv(selected.NOAA.network, file="Selected Meteorological Stations from Normals.csv") #Meteorological information
        #write.csv(station.db, file="EC Meteorological DB.csv")
        #write.csv(ECm.evaporation, file="Evaporation.csv")
        write.csv(ECm.annual.comp, file="Annual compilation.csv")
        write.csv(ECm.monthly.data, file="Monthly compilation.csv")
        write.csv(ECm.monthly.data.nd, file="Monthly non dimensional compilation.csv")
        write.csv(ECm.monthly.mwi, file="Monthly information available.csv")

        #write.csv(ECm.annual.peak.table,file="Annual Daily Precipitation.csv")
        write.csv(ECm.st.totp.comp,file="Daily Precipitation.csv")
}


#' Remove an station from NOAA DB
#'
#' Remove an station from NOAA DB
#'
#' @export
NOAA.rm<-function(x=0){

        #Prepare Matrix
        y<-matrix(T,length(ECm.st.totp),1)

        y[x]<-F

        #Clean matrixes
        selected.NOAA.network<<-selected.NOAA.network[y,]
        ECm.st.totp<<-ECm.st.totp[y]
        #ECm.annual.comp<<-ECm.annual.comp[y]
        NOAA.analysis()

}


ECm.select.min<-function(n=0){ECm.monthly.mwi[ECm.monthly.mwi$Min<n,]$ID}

#' Coast distance for NOAA DB
#'
#' Coast distance for NOAA DB
#'
#' @export
NOAA.coast.distance<-function(){

        library(maps)
        library(maptools)
        library(mapdata)

        Global.map <- data.frame(map('worldHires',interior=FALSE,plot=FALSE)[c('x', 'y')])
        #ggplot(Global.map, aes(x = x, y = y)) +
        #        geom_path() +
        #        xlab(expression(Longitude * ' ' * degree * E * ' ')) +
        #        ylab(expression(Latitude * ' ' * degree * S * ' ')) +
        #        theme_bw()

        ## single point for a simple test

        pts <- as.matrix(selected.NOAA.network[,c(4,3)])

        ## convert coords to matrix and dump NA
        xy.coast <- cbind(Global.map$x, Global.map$y)[!is.na(Global.map$x), ]


        ## container for all the nearest points matching the input
        closest.points <- matrix(0, ncol = 2, nrow = nrow(pts))
        distance<-matrix(0,ncol=1,nrow(pts))
        for (i in 1:nrow(pts)) {
                closest.points[i, 1:2] <- xy.coast[which.min(spDistsN1(xy.coast,
                                                                       pts[i,], longlat = TRUE)), ]
                distance[i,1]<-round(min(spDistsN1(xy.coast, pts[i,], longlat = TRUE)),0)
        }
        elevation<-data.frame(elevation=mapply(GE.elev,longitude=selected.NOAA.network$Longitude,latitude=selected.NOAA.network$Latitude))

        NOAA.stations.syn$annual.avg.table<-cbind(NOAA.stations.syn$annual.avg.table,distance,elevation)
        names(NOAA.stations.syn$annual.avg.table)[c(7,8)]<-c("Coast.Distance","Elev.G.E.")
        NOAA.stations.syn<<-NOAA.stations.syn

}

