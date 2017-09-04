#' Find meteorological information from MERRA (NASA POWER)
#'
#' Find meteorological information from MERRA (NASA POWER)
#'
#' @export
MERRA.text<-function(lat=33.419,long=110.974164){
            library("RCurl", lib.loc="~/R/win-library/3.1") #Network interfase
            paste("http://power.larc.nasa.gov/cgi-bin/hirestimeser.cgi?email=hirestimeser%40larc.nasa.gov&step=1&lat=",
                  lat,"&lon=",long,"&ms=1&ds=1&ys=1981&me=12&de=31&ye=2015&p=MHswv_dwn&p=MHPS&p=MHT2M&p=MHT2MN&p=MHT2MX&p=MHQV2M&p=MHRH2M&p=MHDFP2M&p=MHWS10M&p=MHPRECTOT&submit=Submit",sep="")


}


#' Get meteorological information from MERRA (NASA POWER)
#'
#' Get meteorological information from MERRA (NASA POWER)
#'
#' @export
MERRA.get<-function(lat=33.419,long=110.974164){
            library("RCurl", lib.loc="~/R/win-library/3.1") #Network interfase
            x<-getURL(url=MERRA.text(lat,long), async = TRUE)
            MERRA.file<<-x
}



#' Process meteorological information from MERRA (NASA POWER)
#'
#' Process meteorological information from MERRA (NASA POWER)
#'
#' @export
MERRA.process<-function(x=MERRA.file){
            #x<-MERRA.table

            #Save a temporal file with the info
            MERRA.tb.name<-tempfile()
            save(x,file = MERRA.tb.name)
            MERRA.tb.length<-length(readLines(MERRA.tb.name,warn = FALSE))-4
            MERRA.tb.skip<-27
            MERRA.tb.skip.end<-2

            MERRA.data2<-read.table(text=x,skip=MERRA.tb.skip,
                                    nrow=MERRA.tb.length-(MERRA.tb.skip+MERRA.tb.skip.end+1),
                                    head=F,na.strings = "-",stringsAsFactors = FALSE)

            MERRA.meta1<-read.csv(text=x,skip=1,nrows=4,head=F,as.is = T)
            MERRA.meta2<-read.csv(text=x,skip=6,nrows=11,head=F,as.is = T)
            MERRA.meta<-rbind(MERRA.meta1,MERRA.meta2)

            #MERRA info Preparation

            #HL Correction instead of complete cases when less then 90 days are missing
            a<-rle(complete.cases(MERRA.data2))
            a$values[a$values==F&a$lengths<90]<-T

            MERRA.data1<-MERRA.data2[inverse.rle(a),]

            #Date
            date<-as.Date(paste(MERRA.data1[,1],MERRA.data1[,2],MERRA.data1[,3],sep="-"),format("%Y-%m-%d"))
            #Data Frame
            MERRA.info<-data.frame(date,MERRA.data1[,4:13])#,
            #Names for Data Frame
            names(MERRA.info)<- c("Date","SWV_DWN","PS","T2M","T2MN","T2MX","QV2M","RH2M","DFP2M","WS10M","PRECTOT")

            #HL correction
            MERRA.info$SWV_DWN[is.na(MERRA.info$SWV_DWN)]<-0

            #SWV_DWN     SRB/FLASHFlux 1/2x1/2 Insolation Incident On A Horizontal Surface (kWh/m^2/day)
            #PS          MERRA 1/2x1/2 Atmospheric Pressure (kPa)
            #T2M         MERRA 1/2x1/2 Air Temperature At 2 m Above The Surface Of The Earth (degrees C)
            #T2MN        MERRA 1/2x1/2 Minimum Air Temperature At 2 m Above The Surface Of The Earth (degrees C)
            #T2MX        MERRA 1/2x1/2 Maximum Air Temperature At 2 m Above The Surface Of The Earth (degrees C)
            #QV2M        MERRA 1/2x1/2 Humidity Ratio At 2 m Above The Surface Of The Earth (kg/kg)
            #RH2M        MERRA 1/2x1/2 Relative Humidity (%)
            #DFP2M       MERRA 1/2x1/2 Dew/Frost Point Temperature (degrees C)
            #WS10M       MERRA 1/2x1/2 Wind Speed At 10 m Above The Surface Of The Earth (m/s)
            #PRECTOT     MERRA 1/2x1/2 Precipitation (mm/day)

            write.csv(MERRA.meta,"MERRA metafile.csv")
            write.csv(MERRA.info,"MERRA info.csv")
            MERRA.info
}


#' MERRA evaporation
#'
#' MERRA evaporation
#' @export
MERRA.Evap <-function(Meteo.info=MERRA.table, MAP=NULL,Latitude=39.346, Longitude=34)  {
            library(zoo)
            library(lubridate)
            library(Evapotranspiration)
            library(xlsx)
            library(sirad)

            pp.daily<-zoo(Meteo.info$PRECTOT,Meteo.info$Date)

            monthly.ts<-daily2monthly.V2(x = pp.daily,FUN = sum, na.rm=TRUE,max.miss=5)

            #adjustment for complete year
            pp.monthly.table<-zoo2matrix(monthly.ts)

            #monthly resume
            pp.monthly<-colMeans(pp.monthly.table, na.rm=TRUE)

            if(is.null(MAP)){
                        MAP<-sum(pp.monthly)
                        print(paste("MAP based on MERRA:",round(MAP,0),"mm"))
            } else{

                        MAP<-MAP
                        print(paste("Assumed MAP",round(MAP,0),"mm"))
            }


            Elevation<-GE.elev(longitude = Longitude,latitude = Latitude)
            print(paste("Elevation based on Google Earth:",round(Elevation,0),"masl"))


            #Meteo.info=MERRA.Miami.Patched
            #MAP=470.5;Elevation =1200;Latitude = 33.404
            #units Global Rad: kWh/(m2d), Temp: Celcius Pp: mm, WS: m/s

            #Calculating Extraterrestrial Solar Radiation
            ESR<-extrat(yday(Meteo.info$Date),radians(Latitude))
            #Daily.ESR units MJ/m2
            Daily.ESR<-ESR$ExtraTerrestrialSolarRadiationDaily
            #Daily.ESR units W/m2
            Daily.ESR<-Daily.ESR*1e6/86400
            #Daily.ESR units kWh/(m2d)
            Daily.ESR<-Daily.ESR*0.024

            #Daily.ESR units MJ/(m2d)
            Daily.ESR<-Daily.ESR*3.6


            Daily.Global.Rad<-Meteo.info$SWV_DWN*3.6

            #Meteo.info2<-data.frame(Meteo.info,Daily.ESR,Daily.Global.Rad,ESR$DayLength)

            #From Fleming et al 1989
            a<-0.25
            b<-0.5

            Sunshine.hours.fraction<-(Daily.Global.Rad-a*Daily.ESR)/(b*Daily.ESR)
            Sunshine.hours.fraction[Sunshine.hours.fraction>=1]<-1
            Sunshine.hours.fraction[Sunshine.hours.fraction<=0]<-0
            summary(Sunshine.hours.fraction)
            plot(Sunshine.hours.fraction[1:2000],type = "l")

            Sunshine.hours<-round(ESR$DayLength*Sunshine.hours.fraction,2)

            Meteo.info<-data.frame(Meteo.info,Sunshine.hours)
            #stationid<-1174
            #station.normal<-ECm.normal(stationid=stationid)

            #daysmonth<-c(31,28,31,30,31,30,31,31,30,31,30,31)

            #time<-seq(as.Date("2001/1/1"), by = "month", length.out = 12)

            #Define time series
            #Date.daily<-seq(as.Date("2001/1/1"), by = "day", length.out = 365)
            Date.daily<-Meteo.info$Date

            #Date.monthly<- as.yearmon(2001 + seq(0, 11)/12)
            Date.monthly<-as.yearmon(Meteo.info$Date)
            Date.monthly<-Date.monthly[!duplicated(Date.monthly)]

            #J<-zoo(seq(from = 1, to=365),Date.daily)
            J<-zoo(yday(Meteo.info$Date),Meteo.info$Date)

            #i<-1:12
            i<-month(as.Date(Date.monthly))

            #ndays<-zoo(daysmonth,Date.monthly)
            ndays<-days_in_month(Date.monthly)
            attributes(ndays)<-NULL
            ndays<-zoo(ndays,Date.monthly)

            Tmax<-zoo(Meteo.info$T2MX,Date.daily)

            Tmin<-zoo(Meteo.info$T2MN,Date.daily)

            u2<-NULL
            uz<-zoo(Meteo.info$WS10M,Date.daily)

            n<-zoo(Meteo.info$Sunshine.hours,Date.daily)

            Rs<-zoo(Meteo.info$SWV_DWN*3.6,Date.daily) #transformation from KWh/m2 (MERRA) = 3.6MJ/m2 (SOURCE)

            Cd<-NULL
            Precip<-zoo(Meteo.info$PRECTOT,Date.daily)
            Epan<-NULL


            #Estimation of RHmax and RHmin for Penman Monteith RHmax and RHmin = RHmean

            #         Etmax<-0.6108*exp(17.27*Tmax/(Tmax+237.3))
            #         Etmin<-0.6108*exp(17.27*Tmin/(Tmin+237.3))
            RHmean<-zoo(Meteo.info$RH2M,Date.daily)

            RHmax<-RHmean
            RHmin<-RHmean

            Tdew<-zoo(Meteo.info$DFP2M,Date.daily)

            #Definition of constants
            #Station.info<-ECm.id(stationid)
            data("constants")
            constants$Elev<-Elevation
            constants$lat<-Latitude

            constants$lat_rad<-constants$lat/180*pi
            constants$PA<-MAP

            constants<<-constants

            data2<-list(Date.daily,Date.monthly,J,i,ndays,Tmax,Tmin,u2,uz,Rs,n,Cd,Precip,Epan,RHmax,RHmin, Tdew)
            names(data2)<-c("Date.daily","Date.monthly","J","i","Ndays","Tmax","Tmin","u2","uz","Rs","n","Cd","Precip","Epan","RHmax","RHmin", "Tdew")

            #Morton CRAE Calculation
            class(data2)<-"MortonCRAE"
            funname<<-"MortonCRAE"
            data2<<-data2
            ET.Morton.PET<-ET.MortonCRAE(data2,constants,ts="monthly",est = "potential ET",solar = "data",Tdew = TRUE,alpha=0.08)
            ET.Morton.PETsh<-ET.MortonCRAE(data2,constants,est = "potential ET",solar = "sunshine hours",Tdew = TRUE)

            ET.Morton.AET<-ET.MortonCRAE(data2,constants,est = "actual areal ET",solar = "data",Tdew = TRUE,,alpha=0.08)
            ET.Morton.AETsh<-ET.MortonCRAE(data2,constants,est = "actual areal ET",solar = "sunshine hours",Tdew = TRUE)

            class(data2)<-"MortonCRWE"
            funname<<-"MortonCRWE"
            ET.Morton.PanEvap<-ET.MortonCRWE(data2,constants,est = "potential ET",solar = "data",Tdew = TRUE,alpha=0.08)
            ET.Morton.PanEvapsh<-ET.MortonCRWE(data2,constants,est = "potential ET",solar = "sunshine hours",Tdew = TRUE)
            ET.Morton.LakeEvap<-ET.MortonCRWE(data2,constants,est = "shallow lake ET",solar = "data",Tdew = TRUE,alpha=0.08)
            ET.Morton.LakeEvapsh<-ET.MortonCRWE(data2,constants,est = "shallow lake ET",solar = "sunshine hours",Tdew = TRUE)

            class(data2)<-"PenmanMonteith"
            funname<<-"PenmanMonteith"
            ET.Penman_Montheith.Evap<-ET.PenmanMonteith(data2,constants,solar = "data",wind="yes",crop="short")
            ET.Penman_Montheith.Evapsh<-ET.PenmanMonteith(data2,constants,solar = "sunshine hours",wind="yes",crop="short")
            #Annual Evaporation
            Annual.Evaporation<-data.frame("Morton Actual Area ET.Solar"=ET.Morton.AET$ET.Annual,
                                           "Morton Actual Area ET.Sunshine Hours"=ET.Morton.AETsh$ET.Annual,
                                           "Morton Potential ET.Solar"=ET.Morton.PET$ET.Annual,
                                           "Morton Potential ET.Sunshine Hours"=ET.Morton.PETsh$ET.Annual,
                                           "Morton Lake Evap.Solar"=ET.Morton.LakeEvap$ET.Annual,
                                           "Morton Lake Evap.Sunshine Hours"=ET.Morton.LakeEvapsh$ET.Annual,
                                           "Morton Pan Evap.Solar"=ET.Morton.PanEvap$ET.Annual,
                                           "Morton Pan Evap.Sunshine Hours"=ET.Morton.PanEvapsh$ET.Annual,
                                           "Penman-Montheith ETo.Solar"=ET.Penman_Montheith.Evap$ET.Annual,
                                           "Penman-Montheith ETo.Sunshine Hours"=ET.Penman_Montheith.Evapsh$ET.Annual)

            #Monthly Evaporation
            Monthly.Evaporation<-data.frame("Morton Actual Area ET.Solar"=ET.Morton.AET$ET.Monthly,
                                            "Morton Actual Area ET.Sunshine Hours"=ET.Morton.AETsh$ET.Monthly,
                                            "Morton Potential ET.Solar"=ET.Morton.PET$ET.Monthly,
                                            "Morton Potential ET.Sunshine Hours"=ET.Morton.PETsh$ET.Monthly,
                                            "Morton Lake Evap.Solar"=ET.Morton.LakeEvap$ET.Monthly,
                                            "Morton Lake Evap.Sunshine Hours"=ET.Morton.LakeEvapsh$ET.Monthly,
                                            "Morton Pan Evap.Solar"=ET.Morton.PanEvap$ET.Monthly,
                                            "Morton Pan Evap.Sunshine Hours"=ET.Morton.PanEvapsh$ET.Monthly,
                                            "Penman-Montheith ETo.Solar"=ET.Penman_Montheith.Evap$ET.Monthly,
                                            "Penman-Montheith ETo.Sunshine Hours"=ET.Penman_Montheith.Evapsh$ET.Monthly)

            #Daily Evaporation
            Daily.Evaporation<-data.frame("Penman-Montheith ETo.Solar"=ET.Penman_Montheith.Evap$ET.Daily,
                                          "Penman-Montheith ETo.Sunshine Hours"=ET.Penman_Montheith.Evapsh$ET.Daily)


            filename<-paste("Evaporation MERRA ","Lat",signif(Latitude,3)," Lon",signif(Longitude,3)," ",
                            as.character(as.Date(now())),".xlsx",sep="")
            filename<-gsub("-","_",filename)

            jgc<-function()
            {
                        .jcall("java/lang/System", method = "gc")
            }

            site.df<-data.frame(Longitude=Longitude,Latitude=Latitude,MAP=MAP,Elevation=Elevation)

            jgc()
            write.xlsx2(site.df,file = filename,sheetName = "Site",append = F)
            jgc()
            write.xlsx2(Annual.Evaporation,file = filename,sheetName = "Annual",append = T)
            jgc()
            write.xlsx2(Monthly.Evaporation,file = filename,sheetName = "Monthly",append = T)
            jgc()
            write.xlsx2(Daily.Evaporation,file = filename,sheetName = "Daily",append = T)
            jgc()
            write.xlsx2(Meteo.info,file = filename,sheetName = "MERRA.info",append = T)
            jgc()
            results<-list(site.df,Annual.Evaporation,Monthly.Evaporation,Daily.Evaporation,Meteo.info,
                          ET.Morton.PET,ET.Morton.PETsh,ET.Morton.AET,ET.Morton.AETsh,
                          ET.Morton.PanEvap,ET.Morton.PanEvapsh,ET.Morton.LakeEvap,ET.Morton.LakeEvapsh,
                          ET.Penman_Montheith.Evap,ET.Penman_Montheith.Evapsh)
            names(results)<-c("Site","Annual","Monthly","Daily","MERRA",
                              "ET.Morton.PET.Solar","ET.Morton.PET.Sunshine.Hours",
                              "ET.Morton.AET.Solar","ET.Morton.AET.Sunshine.Hours",
                              "ET.Morton.PanEvap.Solar","ET.Morton.PanEvapsh.Sunshine.Hours","ET.Morton.LakeEvap.Solar",
                              "ET.Morton.LakeEvap.Sunshine.Hours","ET.Penman_Montheith.Evap.Solar","ET.Penman_Montheith.Evap.Sunshine.Hours")

            print(colMeans(results$Annual))
            results
}
