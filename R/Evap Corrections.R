
#' MERRA evaporation for High latitudes
#'
#' MERRA evaporation for High latitudes
#' @export
MERRA.Evap.HL<-function(Meteo.info=MERRA.table, MAP=NULL,Latitude=39.346, Longitude=34)  {
            library(zoo)
            library(lubridate)
           # library(Evapotranspiration)
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

            Meteo.info$Sunshine.hours[is.na(Meteo.info$Sunshine.hours)]<-0

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
            data("constants",package="Evapotranspiration")
            # constants<-NULL
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
            ET.Morton.PET<-ET.MortonCRAE(data=data2,constants,ts="monthly",est = "potential ET",solar = "data",Tdew = TRUE,alpha=0.08)
            ET.Morton.PETsh<-ET.MortonCRAE(data2,constants,est = "potential ET",solar = "sunshine hours",Tdew = TRUE)

            ET.Morton.AET<-ET.MortonCRAE(data2,constants,est = "actual areal ET",solar = "data",Tdew = TRUE,,alpha=0.08)
            ET.Morton.AETsh<-ET.MortonCRAE(data2,constants,est = "actual areal ET",solar = "sunshine hours",Tdew = TRUE)

            class(data2)<-"MortonCRWE"
            funname<<-"MortonCRWE"
            ET.Morton.PanEvap<-ET.MortonCRWE(data2,constants,est = "potential ET",solar = "data",Tdew = TRUE,alpha=0.08)
            ET.Morton.PanEvapsh<-ET.MortonCRWE(data2,constants,est = "potential ET",solar = "sunshine hours",Tdew = TRUE)
            ET.Morton.LakeEvap<-ET.MortonCRWE(data2,constants,est = "shallow lake ET",solar = "data",Tdew = TRUE,alpha=0.08)
            ET.Morton.LakeEvapsh<-ET.MortonCRWE(data2,constants,est = "shallow lake ET",solar = "sunshine hours",Tdew = TRUE)

            # class(data2)<-"PenmanMonteith"
            # funname<<-"PenmanMonteith"
            # ET.Penman_Montheith.Evap<-ET.PenmanMonteith(data2,constants,solar = "data",wind="yes",crop="short")
            # ET.Penman_Montheith.Evapsh<-ET.PenmanMonteith(data2,constants,solar = "sunshine hours",wind="yes",crop="short")
            #Annual Evaporation
            Annual.Evaporation<-data.frame("Morton Actual Area ET.Solar"=ET.Morton.AET$ET.Annual,
                                           "Morton Actual Area ET.Sunshine Hours"=ET.Morton.AETsh$ET.Annual,
                                           "Morton Potential ET.Solar"=ET.Morton.PET$ET.Annual,
                                           "Morton Potential ET.Sunshine Hours"=ET.Morton.PETsh$ET.Annual,
                                           "Morton Lake Evap.Solar"=ET.Morton.LakeEvap$ET.Annual,
                                           "Morton Lake Evap.Sunshine Hours"=ET.Morton.LakeEvapsh$ET.Annual,
                                           "Morton Pan Evap.Solar"=ET.Morton.PanEvap$ET.Annual,
                                           "Morton Pan Evap.Sunshine Hours"=ET.Morton.PanEvapsh$ET.Annual
                                           # "Penman-Montheith ETo.Solar"=ET.Penman_Montheith.Evap$ET.Annual,
                                           # "Penman-Montheith ETo.Sunshine Hours"=ET.Penman_Montheith.Evapsh$ET.Annual
            )

            #Monthly Evaporation
            Monthly.Evaporation<-data.frame("Morton Actual Area ET.Solar"=ET.Morton.AET$ET.Monthly,
                                            "Morton Actual Area ET.Sunshine Hours"=ET.Morton.AETsh$ET.Monthly,
                                            "Morton Potential ET.Solar"=ET.Morton.PET$ET.Monthly,
                                            "Morton Potential ET.Sunshine Hours"=ET.Morton.PETsh$ET.Monthly,
                                            "Morton Lake Evap.Solar"=ET.Morton.LakeEvap$ET.Monthly,
                                            "Morton Lake Evap.Sunshine Hours"=ET.Morton.LakeEvapsh$ET.Monthly,
                                            "Morton Pan Evap.Solar"=ET.Morton.PanEvap$ET.Monthly,
                                            "Morton Pan Evap.Sunshine Hours"=ET.Morton.PanEvapsh$ET.Monthly
                                            # "Penman-Montheith ETo.Solar"=ET.Penman_Montheith.Evap$ET.Monthly,
                                            # "Penman-Montheith ETo.Sunshine Hours"=ET.Penman_Montheith.Evapsh$ET.Monthly
            )

            #Daily Evaporation
            # Daily.Evaporation<-data.frame("Penman-Montheith ETo.Solar"=ET.Penman_Montheith.Evap$ET.Daily,
            #                               "Penman-Montheith ETo.Sunshine Hours"=ET.Penman_Montheith.Evapsh$ET.Daily)


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
            # write.xlsx2(Daily.Evaporation,file = filename,sheetName = "Daily",append = T)
            # jgc()
            write.xlsx2(Meteo.info,file = filename,sheetName = "MERRA.info",append = T)
            jgc()
            results<-list(site.df,Annual.Evaporation,Monthly.Evaporation,Meteo.info,
                          ET.Morton.PET,ET.Morton.PETsh,ET.Morton.AET,ET.Morton.AETsh,
                          ET.Morton.PanEvap,ET.Morton.PanEvapsh,ET.Morton.LakeEvap,ET.Morton.LakeEvapsh
                          # ET.Penman_Montheith.Evap,ET.Penman_Montheith.Evapsh
            )
            names(results)<-c("Site","Annual","Monthly","MERRA",
                              "ET.Morton.PET.Solar","ET.Morton.PET.Sunshine.Hours",
                              "ET.Morton.AET.Solar","ET.Morton.AET.Sunshine.Hours",
                              "ET.Morton.PanEvap.Solar","ET.Morton.PanEvapsh.Sunshine.Hours","ET.Morton.LakeEvap.Solar",
                              "ET.Morton.LakeEvap.Sunshine.Hours"
                              #"ET.Penman_Montheith.Evap.Solar","ET.Penman_Montheith.Evap.Sunshine.Hours"
            )

            print(colMeans(results$Annual))
            results
}


#' ET.PenmanMonteith for High latitudes
#'
#' ET.PenmanMonteith for High latitudes
#' @export
ET.PenmanMonteith<-function (data, constants, ts = "daily", solar = "sunshine hours",
                             wind = "yes", crop = "short", ...)
{
            if (is.null(data$Tmax) | is.null(data$Tmin)) {
                        stop("Required data missing for 'Tmax.daily' and 'Tmin.daily', or 'Temp.subdaily'")
            }
            if (is.null(data$RHmax) | is.null(data$RHmin)) {
                        stop("Required data missing for 'RHmax.daily' and 'RHmin.daily', or 'RH.subdaily'")
            }
            if (wind == "yes") {
                        if (is.null(data$u2) & is.null(data$uz)) {
                                    stop("Required data missing for 'uz.subdaily' or 'u2.subdaily'")
                        }
            }
            if (solar == "data" & is.null(data$Rs)) {
                        stop("Required data missing for 'Rs.daily'")
            }
            else if (solar == "sunshine hours" & is.null(data$n)) {
                        stop("Required data missing for 'n.daily'")
            }
            else if (solar == "cloud" & is.null(data$Cd)) {
                        stop("Required data missing for 'Cd.daily'")
            }
            else if (solar == "monthly precipitation" & is.null(data$Precip)) {
                        stop("Required data missing for 'Precip.daily'")
            }
            if (wind != "yes" & wind != "no") {
                        stop("Please choose if actual data will be used for wind speed from wind = 'yes' and wind = 'no'")
            }
            if (wind == "yes") {
                        if (crop != "short" & crop != "tall") {
                                    stop("Please enter 'short' or 'tall' for the desired reference crop type")
                        }
                        else {
                                    alpha <- 0.23
                                    if (crop == "short") {
                                                z0 <- 0.02
                                    }
                                    else {
                                                z0 <- 0.1
                                    }
                        }
            }
            else {
                        z0 <- 0.02
                        alpha <- 0.25
            }
            Ta <- (data$Tmax + data$Tmin)/2
            vs_Tmax <- 0.6108 * exp(17.27 * data$Tmax/(data$Tmax + 237.3))
            vs_Tmin <- 0.6108 * exp(17.27 * data$Tmin/(data$Tmin + 237.3))
            vas <- (vs_Tmax + vs_Tmin)/2
            vabar <- (vs_Tmin * data$RHmax/100 + vs_Tmax * data$RHmin/100)/2
            P <- 101.3 * ((293 - 0.0065 * constants$Elev)/293)^5.26
            delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta +
                                                                                    237.3)^2)
            gamma <- 0.00163 * P/constants$lambda
            d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J)
            delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39)
            w_s <- acos(pmax(pmin(-tan(constants$lat_rad) * tan(delta2),1),-1))
            N <- 24/pi * w_s
            R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(constants$lat_rad) *
                                                                   sin(delta2) + cos(constants$lat_rad) * cos(delta2) *
                                                                   sin(w_s))
            R_so <- (0.75 + (2 * 10^-5) * constants$Elev) * R_a
            if (solar == "data") {
                        R_s <- data$Rs
            } else if (solar != "monthly precipitation") {
                        R_s <- (constants$as + constants$bs * (data$n/N)) *
                                    R_a
            } else {
                        R_s <- (0.85 - 0.047 * data$Cd) * R_a
            }
            R_nl <- constants$sigma * (0.34 - 0.14 * sqrt(vabar)) *
                        ((data$Tmax + 273.2)^4 + (data$Tmin + 273.2)^4)/2 *
                        (1.35 * R_s/R_so - 0.35)
            R_nsg <- (1 - alpha) * R_s
            R_ng <- R_nsg - R_nl
            if (wind == "yes") {
                        if (is.null(data$u2)) {
                                    u2 <- data$uz * 4.87/log(67.8 * constants$z - 5.42)
                        }
                        else {
                                    u2 <- data$u2
                        }
                        if (crop == "short") {
                                    r_s <- 70
                                    CH <- 0.12
                                    ET_RC.Daily <- (0.408 * delta * (R_ng - constants$G) +
                                                                gamma * 900 * u2 * (vas - vabar)/(Ta + 273))/(delta +
                                                                                                                          gamma * (1 + 0.34 * u2))
                        }
                        else {
                                    r_s <- 45
                                    CH <- 0.5
                                    ET_RC.Daily <- (0.408 * delta * (R_ng - constants$G) +
                                                                gamma * 1600 * u2 * (vas - vabar)/(Ta + 273))/(delta +
                                                                                                                           gamma * (1 + 0.38 * u2))
                        }
            }
            else {
                        RHmean <- (data$RHmax + data$RHmin)/2
                        R_s.Monthly <- aggregate(R_s, as.yearmon(data$Date.daily,
                                                                 "%m/%y"), mean)
                        R_a.Monthly <- aggregate(R_a, as.yearmon(data$Date.daily,
                                                                 "%m/%y"), mean)
                        Ta.Monthly <- aggregate(Ta, as.yearmon(data$Date.daily,
                                                               "%m/%y"), mean)
                        RHmean.Monthly <- aggregate(RHmean, as.yearmon(data$Date.daily,
                                                                       "%m/%y"), mean)
                        ET_RC.Daily <- matrix(NA, length(data$date.Daily), 1)
                        ET_RC.Monthly <- 0.038 * R_s.Monthly * sqrt(Ta.Monthly +
                                                                                9.5) - 2.4 * (R_s.Monthly/R_a.Monthly)^2 + 0.075 *
                                    (Ta.Monthly + 20) * (1 - RHmean.Monthly/100)
            }
            ET.Daily <- ET_RC.Daily
            if (is.na(mean(ET_RC.Daily))) {
                        ET_RC.Daily <- data$Tmax
                        for (cont in 1:length(data$i)) {
                                    ET_RC.Daily[(((as.numeric(as.yearmon(time(ET_RC.Daily)))) -
                                                              floor(as.numeric(as.yearmon(time(ET_RC.Daily))))) *
                                                             12 + 1) == data$i[cont]] <- ET_RC.Monthly[cont]
                        }
                        ET.Daily <- ET_RC.Daily
                        ET.Monthly <- aggregate(ET.Daily, as.yearmon(data$Date.daily,
                                                                     "%m/%y"), FUN = sum)
                        ET.Annual <- aggregate(ET.Daily, floor(as.numeric(as.yearmon(data$Date.monthly,
                                                                                     "%m/%y"))), FUN = sum)
            }
            else {
                        ET.Monthly <- aggregate(ET.Daily, as.yearmon(data$Date.daily,
                                                                     "%m/%y"), FUN = sum)
                        ET.Annual <- aggregate(ET.Daily, floor(as.numeric(as.yearmon(data$Date.daily,
                                                                                     "%m/%y"))), FUN = sum)
            }
            ET.MonthlyAve <- ET.AnnualAve <- NULL
            for (mon in min(as.POSIXlt(data$Date.daily)$mon):max(as.POSIXlt(data$Date.daily)$mon)) {
                        i = mon - min(as.POSIXlt(data$Date.daily)$mon) + 1
                        ET.MonthlyAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$mon ==
                                                                      mon])
            }
            for (year in min(as.POSIXlt(data$Date.daily)$year):max(as.POSIXlt(data$Date.daily)$year)) {
                        i = year - min(as.POSIXlt(data$Date.daily)$year) + 1
                        ET.AnnualAve[i] <- mean(ET.Daily[as.POSIXlt(data$Date.daily)$year ==
                                                                     year])
            }
            if (wind == "no") {
                        ET_formulation <- "Penman-Monteith (without wind data)"
                        ET_type <- "Reference Crop ET"
                        Surface <- paste("short grass, albedo =", alpha, "; roughness height =",
                                         z0, "m")
            }
            else {
                        if (crop == "short") {
                                    ET_formulation <- "Penman-Monteith FAO56"
                                    ET_type <- "Reference Crop ET"
                                    Surface <- paste("FAO-56 hypothetical short grass, albedo =",
                                                     alpha, "; surface resisitance =", r_s, "sm^-1; crop height =",
                                                     CH, " m; roughness height =", z0, "m")
                        }
                        else {
                                    ET_formulation <- "Penman-Monteith ASCE-EWRI Standardised"
                                    ET_type <- "Reference Crop ET"
                                    Surface <- paste("ASCE-EWRI hypothetical tall grass, albedo =",
                                                     alpha, "; surface resisitance =", r_s, "sm^-1; crop height =",
                                                     CH, " m; roughness height =", z0, "m")
                        }
            }
            if (solar == "data") {
                        message1 <- "Solar radiation data have been used directly for calculating evapotranspiration"
            }
            else if (solar == "sunshine hours") {
                        message1 <- "Sunshine hour data have been used for calculating incoming solar radiation"
            }
            else if (solar == "cloud") {
                        message1 <- "Cloudiness data have been used for calculating sunshine hour and thus incoming solar radiation"
            }
            else {
                        message1 <- "Monthly precipitation data have been used for calculating incoming solar radiation"
            }
            if (wind == "yes") {
                        message2 <- "Wind data have been used for calculating the reference crop evapotranspiration"
            }
            else {
                        message2 <- "Alternative calculation for reference crop evapotranspiration without wind data have been performed"
            }
            message(ET_formulation, " ", ET_type)
            message("Evaporative surface: ", Surface)
            message(message1)
            message(message2)
            results <- list(ET.Daily = ET.Daily, ET.Monthly = ET.Monthly,
                            ET.Annual = ET.Annual, ET.MonthlyAve = ET.MonthlyAve,
                            ET.AnnualAve = ET.AnnualAve, ET_formulation = ET_formulation,
                            ET_type = ET_type, message1 = message1, message2 = message2)
            if (ts == "daily") {
                        res_ts <- ET.Daily
            }
            else if (ts == "monthly") {
                        res_ts <- ET.Monthly
            }
            else if (ts == "annual") {
                        res_ts <- ET.Annual
            }
            message("Timestep: ", ts)
            message("Units: mm")
            message("Time duration: ", time(res_ts[1]), " to ", time(res_ts[length(res_ts)]))
            if (NA %in% res_ts) {
                        message(length(res_ts), " ET estimates obtained; ",
                                length(which(is.na(res_ts))), " NA output entries due to missing data")
                        message("Basic stats (NA excluded)")
                        message("Mean: ", round(mean(res_ts, na.rm = T), digits = 2))
                        message("Max: ", round(max(res_ts, na.rm = T), digits = 2))
                        message("Min: ", round(min(res_ts, na.rm = T), digits = 2))
            }
            else {
                        message(length(res_ts), " ET estimates obtained")
                        message("Basic stats")
                        message("Mean: ", round(mean(res_ts), digits = 2))
                        message("Max: ", round(max(res_ts), digits = 2))
                        message("Min: ", round(min(res_ts), digits = 2))
            }
            for (i in 1:length(results)) {
                        namer <- names(results[i])
                        write.table(as.character(namer), file = "ET_PenmanMonteith.csv",
                                    dec = ".", quote = FALSE, col.names = FALSE, row.names = F,
                                    append = TRUE, sep = ",")
                        write.table(data.frame(get(namer, results)), file = "ET_PenmanMonteith.csv",
                                    col.names = F, append = T, sep = ",")
            }
            invisible(results)
}



#' ET.MortonCRWE for High latitudes
#'
#' ET.MortonCRWE for High latitudes
#' @export
ET.MortonCRWE<-function (data, constants, ts = "monthly", est = "potential ET",
                         solar = "sunshine hours", Tdew = TRUE, alpha = NULL, ...)
{
            constants$epsilonMo <- 0.97
            constants$fz <- 25
            constants$b0 <- 1.12
            constants$b1 <- 13
            constants$b2 <- 1.12
            alpha_zz <- 0.05
            variables <- Radiation(data, constants, ts, solar, Tdew,
                                   alpha)
            R_W <- (1 - variables$alpha_Mo) * variables$G_Mo - variables$B_Mo
            R_TC <- as.vector(R_W)
            for (i in 1:length(R_TC)) {
                        if (R_TC[i] < 0) {
                                    R_TC[i] <- 0
                        }
                        else {
                                    R_TC[i] <- R_TC[i]
                        }
            }
            xiMo <- 1/(0.28 * (1 + variables$vD_Mo/variables$v_Mo) +
                                   R_TC * variables$deltaMo/(variables$ptops * constants$gammaps *
                                                                         (1/variables$ptops)^0.5 * constants$b0 * constants$fz *
                                                                         (variables$v_Mo - variables$vD_Mo)))
            for (i in 1:length(xiMo)) {
                        if (xiMo[i] < 1) {
                                    xiMo[i] <- 1
                        }
                        else {
                                    xiMo[i] <- xiMo[i]
                        }
            }
            f_T <- (1/variables$ptops)^0.5 * constants$fz/xiMo
            lambdaMo1 <- constants$gammaps * variables$ptops + 4 * constants$epsilonMo *
                        constants$sigmaMo * (variables$T_Mo + 274)^3/f_T
            T_p <- variables$T_Mo
            for (i in 1:99999) {
                        v_p <- 6.11 * exp((constants$alphaMo * T_p)/(T_p + constants$betaMo))
                        delta_p <- constants$alphaMo * constants$betaMo * v_p/((T_p +
                                                                                            constants$betaMo)^2)
                        delta_T_p <- (R_W/f_T + variables$vD_Mo - v_p + lambdaMo1 *
                                                  (variables$T_Mo - T_p))/(delta_p + lambdaMo1)
                        T_p <- T_p + delta_T_p
                        if (abs(max(na.omit(delta_T_p))) < 0.01)
                                    break
            }
            v_p <- 6.11 * exp((constants$alphaMo * T_p)/(T_p + constants$betaMo))
            delta_p <- constants$alphaMo * constants$betaMo * v_p/((T_p +
                                                                                constants$betaMo)^2)
            E_P.temp <- R_W - lambdaMo1 * f_T * (T_p - variables$T_Mo)
            R_P <- E_P.temp + variables$ptops * constants$gammaps *
                        f_T * (T_p - variables$T_Mo)
            E_W.temp <- constants$b1 + constants$b2 * R_P/(1 + variables$ptops *
                                                                       constants$gammaps/delta_p)
            E_T_Mo.temp <- 2 * E_W.temp - E_P.temp
            E_P.temp <- 1/(constants$lambdaMo) * E_P.temp
            E_W.temp <- 1/(constants$lambdaMo) * E_W.temp
            E_T_Mo.temp <- 1/(constants$lambdaMo) * E_T_Mo.temp
            E_P <- E_P.temp * data$Ndays
            E_W <- E_W.temp * data$Ndays
            E_T_Mo <- E_T_Mo.temp * data$Ndays
            if (est == "potential ET") {
                        ET_Mo.Monthly <- E_P
                        ET_Mo.Average <- E_P.temp
                        ET_type <- "Potential ET"
            }
            else if (est == "shallow lake ET") {
                        ET_Mo.Monthly <- E_W
                        ET_Mo.Average <- E_W.temp
                        ET_type <- "Shallow Lake Evaporation"
            }
            ET.Daily <- NULL
            ET.Monthly <- ET_Mo.Monthly
            ET.Annual <- aggregate(ET.Monthly, floor(as.numeric(as.yearmon(data$Date.monthly,
                                                                           "%m/%y"))), FUN = sum)
            ET.MonthlyAve <- ET.AnnualAve <- NULL
            for (mon in min(as.POSIXlt(data$Date.monthly)$mon):max(as.POSIXlt(data$Date.monthly)$mon)) {
                        i = mon - min(as.POSIXlt(data$Date.monthly)$mon) + 1
                        ET.MonthlyAve[i] <- mean(ET_Mo.Average[as.POSIXlt(data$Date.monthly)$mon ==
                                                                           mon])
            }
            for (year in min(as.POSIXlt(data$Date.monthly)$year):max(as.POSIXlt(data$Date.monthly)$year)) {
                        i = year - min(as.POSIXlt(data$Date.monthly)$year) +
                                    1
                        ET.AnnualAve[i] <- mean(ET_Mo.Average[as.POSIXlt(data$Date.monthly)$year ==
                                                                          year])
            }
            ET_formulation <- "Morton CRWE"
            message(ET_formulation, " ", ET_type)
            message(variables$message1)
            message(variables$message6)
            results <- list(ET.Daily = ET.Daily, ET.Monthly = ET.Monthly,
                            ET.Annual = ET.Annual, ET.MonthlyAve = ET.MonthlyAve,
                            ET.AnnualAve = ET.AnnualAve, ET_formulation = ET_formulation,
                            ET_type = ET_type, message1 = variables$message1, message6 = variables$message6)
            if (ts == "monthly") {
                        res_ts <- ET.Monthly
            }
            else if (ts == "annual") {
                        res_ts <- ET.Annual
            }
            message("Timestep: ", ts)
            message("Units: mm")
            message("Time duration: ", time(res_ts[1]), " to ", time(res_ts[length(res_ts)]))
            if (NA %in% res_ts) {
                        message(length(res_ts), " ET estimates obtained; ",
                                length(which(is.na(res_ts))), " NA output entries due to missing data")
                        message("Basic stats (NA excluded)")
                        message("Mean: ", round(mean(res_ts, na.rm = T), digits = 2))
                        message("Max: ", round(max(res_ts, na.rm = T), digits = 2))
                        message("Min: ", round(min(res_ts, na.rm = T), digits = 2))
            }
            else {
                        message(length(res_ts), " ET estimates obtained")
                        message("Basic stats")
                        message("Mean: ", round(mean(res_ts), digits = 2))
                        message("Max: ", round(max(res_ts), digits = 2))
                        message("Min: ", round(min(res_ts), digits = 2))
            }
            for (i in 1:length(results)) {
                        namer <- names(results[i])
                        write.table(as.character(namer), file = "ET_MortonCRWE.csv",
                                    dec = ".", quote = FALSE, col.names = FALSE, row.names = F,
                                    append = TRUE, sep = ",")
                        write.table(data.frame(get(namer, results)), file = "ET_MortonCRWE.csv",
                                    col.names = F, append = T, sep = ",")
            }
            invisible(results)
}

#' ET.MortonCRAE for High latitudes
#'
#' ET.MortonCRAE for High latitudes
#' @export
ET.MortonCRAE<-function (data, constants, ts = "monthly", est = "potential ET",
                         solar = "sunshine hours", Tdew = TRUE, alpha = NULL, ...)
{
            variables<- Radiation(data, constants, ts, solar, Tdew,
                                   alpha)
            R_T <- (1 - variables$alpha_Mo) * variables$G_Mo - variables$B_Mo
            R_TC <- as.vector(R_T)
            for (i in 1:length(R_TC)) {
                        if (R_TC[i] < 0) {
                                    R_TC[i] <- 0
                        }
                        else {
                                    R_TC[i] <- R_TC[i]
                        }
            }
            xiMo <- 1/(0.28 * (1 + variables$vD_Mo/variables$v_Mo) +
                                   R_TC * variables$deltaMo/(variables$ptops * constants$gammaps *
                                                                         (1/variables$ptops)^0.5 * constants$b0 * constants$fz *
                                                                         (variables$v_Mo - variables$vD_Mo)))
            for (i in 1:length(xiMo)) {
                        if (xiMo[i] < 1) {
                                    xiMo[i] <- 1
                        }
                        else {
                                    xiMo[i] <- xiMo[i]
                        }
            }
            f_T <- (1/variables$ptops)^0.5 * constants$fz/xiMo
            lambdaMo1 <- constants$gammaps * variables$ptops + 4 * constants$epsilonMo *
                        constants$sigmaMo * (variables$T_Mo + 274)^3/f_T
            T_p <- variables$T_Mo
            for (i in 1:99999) {
                        v_p <- 6.11 * exp((constants$alphaMo * T_p)/(T_p + constants$betaMo))
                        delta_p <- constants$alphaMo * constants$betaMo * v_p/((T_p +
                                                                                            constants$betaMo)^2)
                        delta_T_p <- (R_T/f_T + variables$vD_Mo - v_p + lambdaMo1 *
                                                  (variables$T_Mo - T_p))/(delta_p + lambdaMo1)
                        T_p <- T_p + delta_T_p
                        if (abs(max(na.omit(delta_T_p))) < 0.01)
                                    break
            }
            v_p <- 6.11 * exp((constants$alphaMo * T_p)/(T_p + constants$betaMo))
            delta_p <- constants$alphaMo * constants$betaMo * v_p/((T_p +
                                                                                constants$betaMo)^2)
            E_TP.temp <- R_T - lambdaMo1 * f_T * (T_p - variables$T_Mo)
            R_TP <- E_TP.temp + variables$ptops * constants$gammaps *
                        f_T * (T_p - variables$T_Mo)
            E_TW.temp <- constants$b1 + constants$b2 * R_TP/(1 + variables$ptops *
                                                                         constants$gammaps/delta_p)
            E_T_Mo.temp <- 2 * E_TW.temp - E_TP.temp
            E_TP.temp <- 1/(constants$lambdaMo) * E_TP.temp
            E_TW.temp <- 1/(constants$lambdaMo) * E_TW.temp
            E_T_Mo.temp <- 1/(constants$lambdaMo) * E_T_Mo.temp
            E_TP <- E_TP.temp * data$Ndays
            E_TW <- E_TW.temp * data$Ndays
            E_T_Mo <- E_T_Mo.temp * data$Ndays
            if (est == "potential ET") {
                        ET_Mo.Monthly <- E_TP
                        ET_Mo.Average <- E_TP.temp
                        ET_type <- "Potential ET"
            }
            else if (est == "wet areal ET") {
                        ET_Mo.Monthly <- E_TW
                        ET_Mo.Average <- E_TW.temp
                        ET_type <- "Wet-environment Areal ET"
            }
            else if (est == "actual areal ET") {
                        ET_Mo.Monthly <- E_T_Mo
                        ET_Mo.Average <- E_T_Mo.temp
                        ET_type <- "Actual Areal ET"
            }
            ET.Daily <- NULL
            ET.Monthly <- ET_Mo.Monthly
            ET.Annual <- aggregate(ET.Monthly, floor(as.numeric(as.yearmon(data$Date.monthly,
                                                                           "%m/%y"))), FUN = sum)
            ET.MonthlyAve <- ET.AnnualAve <- NULL
            for (mon in min(as.POSIXlt(data$Date.monthly)$mon):max(as.POSIXlt(data$Date.monthly)$mon)) {
                        i = mon - min(as.POSIXlt(data$Date.monthly)$mon) + 1
                        ET.MonthlyAve[i] <- mean(ET_Mo.Average[as.POSIXlt(data$Date.monthly)$mon ==
                                                                           mon])
            }
            for (year in min(as.POSIXlt(data$Date.monthly)$year):max(as.POSIXlt(data$Date.monthly)$year)) {
                        i = year - min(as.POSIXlt(data$Date.monthly)$year) +
                                    1
                        ET.AnnualAve[i] <- mean(ET_Mo.Average[as.POSIXlt(data$Date.monthly)$year ==
                                                                          year])
            }
            ET_formulation <- "Morton CRAE"
            message(ET_formulation, " ", ET_type)
            message(variables$message1)
            message(variables$message6)
            results <- list(ET.Daily = ET.Daily, ET.Monthly = ET.Monthly,
                            ET.Annual = ET.Annual, ET.MonthlyAve = ET.MonthlyAve,
                            ET.AnnualAve = ET.AnnualAve, ET_formulation = ET_formulation,
                            ET_type = ET_type, message1 = variables$message1, message6 = variables$message6)
            if (ts == "monthly") {
                        res_ts <- ET.Monthly
            }
            else if (ts == "annual") {
                        res_ts <- ET.Annual
            }
            message("Timestep: ", ts)
            message("Units: mm")
            message("Time duration: ", time(res_ts[1]), " to ", time(res_ts[length(res_ts)]))
            if (NA %in% res_ts) {
                        message(length(res_ts), " ET estimates obtained; ",
                                length(which(is.na(res_ts))), " NA output entries due to missing data")
                        message("Basic stats (NA excluded)")
                        message("Mean: ", round(mean(res_ts, na.rm = T), digits = 2))
                        message("Max: ", round(max(res_ts, na.rm = T), digits = 2))
                        message("Min: ", round(min(res_ts, na.rm = T), digits = 2))
            }
            else {
                        message(length(res_ts), " ET estimates obtained")
                        message("Basic stats")
                        message("Mean: ", round(mean(res_ts), digits = 2))
                        message("Max: ", round(max(res_ts), digits = 2))
                        message("Min: ", round(min(res_ts), digits = 2))
            }
            for (i in 1:length(results)) {
                        namer <- names(results[i])
                        write.table(as.character(namer), file = "ET_MortonCRAE.csv",
                                    dec = ".", quote = FALSE, col.names = FALSE, row.names = F,
                                    append = TRUE, sep = ",")
                        write.table(data.frame(get(namer, results)), file = "ET_MortonCRAE.csv",
                                    col.names = F, append = T, sep = ",")
            }
            invisible(results)
}
#' Radiation for Morton's methodologies for High latitudes
#'
#' Radiation for Morton's methodologies for High latitudes
#' @export
Radiation<-function (data, constants, ts = "monthly", solar = "sunshine hours",
                     Tdew = T, alpha = NULL)
{
            if (ts == "daily") {
                        stop("Error: Morton models are not available for daily time step")
            }
            if (is.null(data$Tmax)) {
                        stop("Required data missing for 'Tmax.daily' or 'Temp.subdaily'")
            }
            if (is.null(data$Tmin)) {
                        stop("Required data missing for 'Tmin.daily' or 'Temp.subdaily'")
            }
            if (Tdew == TRUE & is.null(data$Tdew)) {
                        stop("Required data missing for 'Tdew.subdaily'")
            }
            if (Tdew == FALSE & (is.null(data$RHmax) | is.null(data$RHmin))) {
                        stop("Required data missing for 'RHmax.daily' and 'RHmin.daily', or 'RH.subdaily'")
            }
            if (is.null(data$n)) {
                        stop("Required data missing for 'n.daily'")
            }
            if (solar == "monthly precipitation") {
                        stop("Only 'data', 'sunshine hours' and 'cloud' are accepted because estimations of sunshine hours is required")
            }
            if (is.null(data$Precip)) {
                        if ("PA" %in% names(constants) == FALSE) {
                                    stop("Required data missing for 'Precip.daily' or required constant missing for 'PA'")
                        }
            }
            T_Mo.temp <- (data$Tmax + data$Tmin)/2
            T_Mo <- aggregate(T_Mo.temp, as.yearmon(data$Date.daily,
                                                    "%m/%y"), FUN = mean)
            if (Tdew == TRUE) {
                        Tdew_Mo <- aggregate(data$Tdew, as.yearmon(data$Date.daily,
                                                                   "%m/%y"), FUN = mean)
            } else {
                        vs_Tmax <- 0.6108 * exp(17.27 * data$Tmax/(data$Tmax +
                                                                               237.3))
                        vs_Tmin <- 0.6108 * exp(17.27 * data$Tmin/(data$Tmin +
                                                                               237.3))
                        vas <- (vs_Tmax + vs_Tmin)/2
                        vabar <- (vs_Tmin * data$RHmax/100 + vs_Tmax * data$RHmin/100)/2
                        vabar_Mo <- aggregate(vabar, as.yearmon(data$Date.daily,
                                                                "%m/%y"), FUN = mean)
                        Tdew_Mo <- (116.9 + 237.3 * log(vabar_Mo))/(16.78 -
                                                                                log(vabar_Mo))
            }
            delta <- 4098 * (0.6108 * exp(17.27 * T_Mo/(T_Mo + 237.3)))/(T_Mo +
                                                                                     237.3)^2
            deltas <- 0.409 * sin(2 * pi/365 * data$J - 1.39)
            omegas <- acos(pmin(pmax(-tan(constants$lat_rad) * tan(deltas),-1),1))

            omegas[omegas==0]<-0.0001

            if (solar == "sunshine hours") {
                        N <- 24/pi * omegas
                        S_daily <- data$n/N
                        for (i in 1:length(S_daily)) {
                                    if (S_daily[i] > 1) {
                                                S_daily[i] <- 1
                                    }
                        }
                        S <- mean(S_daily)
                        if ("PA" %in% names(constants) == TRUE) {
                                    PA <- constants$PA
                        } else {
                                    PA <- mean(aggregate(data$Precip, floor(as.numeric(as.yearmon(data$Date.daily,
                                                                                                  "%m/%y"))), FUN = sum))
                        }
                        if (class(data) == "MortonCRLE" | class(data) == "MortonCRWE") {
                                    constants$epsilonMo <- 0.97
                                    constants$fz <- 25
                                    constants$b0 <- 1.12
                                    constants$b1 <- 13
                                    constants$b2 <- 1.12
                        }
                        ptops <- ((288 - 0.0065 * constants$Elev)/288)^5.256
                        alpha_zd <- 0.26 - 0.00012 * PA * sqrt(ptops) * (1 +
                                                                                     abs(constants$lat/42) + (constants$lat/42)^2)
                        if (alpha_zd < 0.11) {
                                    alpha_zd <- 0.11
                        }  else {
                                    if (alpha_zd > 0.17) {
                                                alpha_zd <- 0.17
                                    } else {
                                                alpha_zd <- alpha_zd
                                    }
                        }
                        vD_Mo <- 6.11 * exp(constants$alphaMo * Tdew_Mo/(Tdew_Mo +
                                                                                     constants$betaMo))
                        v_Mo <- 6.11 * exp(constants$alphaMo * T_Mo/(T_Mo +
                                                                                 constants$betaMo))
                        deltaMo <- constants$alphaMo * constants$betaMo * v_Mo/((T_Mo +
                                                                                             constants$betaMo)^2)
                        thetaMo <- (23.2 * sin((29.5 * data$i - 94) * pi/180)) *
                                    pi/180
                        Z_Mo <- acos(cos(constants$lat_rad - thetaMo))
                        for (i in 1:length(Z_Mo)) {
                                    if (cos(Z_Mo[i]) < 0.001) {
                                                Z_Mo[i] <- acos(0.001)
                                    }
                        }
                        omegaMo <- acos(pmax(pmin(1 - cos(Z_Mo)/(cos(constants$lat_rad) * cos(thetaMo)),1),-1))
                        cosz <- cos(Z_Mo) + (sin(omegaMo)/omegaMo - 1) * cos(constants$lat_rad) *
                                    cos(thetaMo)
                        etaMo <- 1 + 1/60 * sin((29.5 * data$i - 106) * pi/180)
                        G_E <- 1354/(etaMo^2) * omegaMo/pi * cosz
                        alpha_zz <- matrix(NA, length(v_Mo), 1)
                        alpha_zz[1:length(v_Mo)] <- alpha_zd
                        for (i in 1:length(v_Mo)) {
                                    if (alpha_zz[i] < 0.11) {
                                                alpha_zz[i] <- 0.11
                                    }                                    else {
                                                if (alpha_zz[i] > 0.5 * (0.91 - vD_Mo[i]/v_Mo[i])) {
                                                            alpha_zz[i] <- 0.91 - vD_Mo[i]/v_Mo[i]
                                                }  else {
                                                }
                                    }
                        }
                        if (class(data) == "MortonCRLE" | class(data) == "MortonCRWE") {
                                    alpha_zz[1:length(v_Mo)] <- 0.05
                        }
                        c_0 <- as.vector(v_Mo - vD_Mo)
                        for (i in 1:length(c_0)) {
                                    if (c_0[i] < 0) {
                                                c_0[i] <- 0
                                    }                                    else {
                                                if (c_0[i] > 1) {
                                                            c_0[i] <- 1
                                                }  else {
                                                            c_0[i] <- c_0[i]
                                                }
                                    }
                        }
                        alpha_z <- alpha_zz + (1 - c_0^2) * (0.34 - alpha_zz)
                        alpha_0 <- alpha_z * (exp(1.08) - ((2.16 * cos(Z_Mo))/pi +
                                                                       sin(Z_Mo)) * exp(0.012 * Z_Mo * 180/pi))/(1.473 *
                                                                                                                             (1 - sin(Z_Mo)))
                        W_Mo <- vD_Mo/(0.49 + T_Mo/129)
                        c_1 <- as.vector(21 - T_Mo)
                        for (i in 1:length(c_1)) {
                                    if (c_1[i] < 0) {
                                                c_1[i] <- 0
                                    }  else {
                                                if (c_1[i] > 5) {
                                                            c_1[i] <- 5
                                                } else {
                                                            c_1[i] <- c_1[i]
                                                }
                                    }
                        }
                        j_Mo <- (0.5 + 2.5 * (cosz)^2) * exp(c_1 * (ptops -
                                                                                1))
                        tauMo <- exp(-0.089 * (ptops * 1/cosz)^0.75 - 0.083 *
                                                 (j_Mo/cosz)^0.9 - 0.029 * (W_Mo/cosz)^0.6)
                        tauaMo <- as.vector(exp(-0.0415 * (j_Mo/cosz)^0.9 -
                                                            (0.0029)^0.5 * (W_Mo/cosz)^0.3))
                        for (i in 1:length(tauaMo)) {
                                    if (tauaMo[i] < exp(-0.0415 * (as.matrix(j_Mo/cosz)[i])^0.9 -
                                                        0.029 * (as.matrix(W_Mo/cosz)[i])^0.6)) {
                                                tauaMo[i] <- exp(-0.0415 * (as.matrix(j_Mo/cosz)[i])^0.9 -
                                                                             0.029 * (as.matrix(W_Mo/cosz)[i])^0.6)
                                    }else {
                                                tauaMo[i] <- tauaMo[i]
                                    }
                        }
                        G_0 <- G_E * tauMo * (1 + (1 - tauMo/tauaMo) * (1 +
                                                                                    alpha_0 * tauMo))
                        G_Mo <- S * G_0 + (0.08 + 0.3 * S) * (1 - S) * G_E
                        alpha_Mo <- alpha_0 * (S + (1 - S) * (1 - Z_Mo/330 *
                                                                          180/pi))
                        c_2 <- as.vector(10 * (vD_Mo/v_Mo - S - 0.42))
                        for (i in 1:length(c_2)) {
                                    if (c_2[i] < 0) {
                                                c_2[i] <- 0
                                    } else {
                                                if (c_2[i] > 1) {
                                                            c_2[i] <- 1
                                                }else {
                                                            c_2[i] <- c_2[i]
                                                }
                                    }
                        }
                        rouMo <- 0.18 * ((1 - c_2) * (1 - S)^2 + c_2 * (1 -
                                                                                    S)^0.5) * 1/ptops
                        B_Mo <- as.vector(constants$epsilonMo * constants$sigmaMo *
                                                      (T_Mo + 273)^4 * (1 - (0.71 + 0.007 * vD_Mo * ptops) *
                                                                                    (1 + rouMo)))
                        for (i in 1:length(B_Mo)) {
                                    if (B_Mo[i] < 0.05 * constants$epsilonMo * constants$sigmaMo *
                                        (T_Mo[i] + 274)^4) {
                                                B_Mo[i] <- 0.05 * constants$epsilonMo * constants$sigmaMo *
                                                            (T_Mo[i] + 274)^4
                                    }                                    else {
                                                B_Mo[i] <- B_Mo[i]
                                    }
                        }
            }  else if (solar == "data") {
                        alpha_Mo = alpha
                        vD_Mo <- 6.11 * exp(constants$alphaMo * Tdew_Mo/(Tdew_Mo +
                                                                                     constants$betaMo))
                        v_Mo <- 6.11 * exp(constants$alphaMo * T_Mo/(T_Mo +
                                                                                 constants$betaMo))
                        ptops <- ((288 - 0.0065 * constants$Elev)/288)^5.256
                        deltaMo <- constants$alphaMo * constants$betaMo * v_Mo/((T_Mo +
                                                                                             constants$betaMo)^2)
                        G_E = NULL
                        R_s <- data$Rs
                        G_Mo <- aggregate(R_s * 10^6/86400, as.yearmon(data$Date.daily,
                                                                       "%m/%y"), FUN = mean)
                        S = NULL
                        Ta <- (data$Tmax + data$Tmin)/2
                        P <- 101.3 * ((293 - 0.0065 * constants$Elev)/293)^5.26
                        delta <- 4098 * (0.6108 * exp((17.27 * Ta)/(Ta + 237.3)))/((Ta +
                                                                                                237.3)^2)
                        gamma <- 0.00163 * P/constants$lambda
                        d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J)
                        delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39)
                        w_s <- acos(pmin(pmax(-tan(constants$lat_rad) * tan(delta2),-1),1))

                        N <- 24/pi * w_s
                        R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(constants$lat_rad) *
                                                                               sin(delta2) + cos(constants$lat_rad) * cos(delta2) *
                                                                               sin(w_s))
                        #Remove of zero values
                        R_a[R_a==0.000]<-0.0001

                        R_so <- (0.75 + (2 * 10^-5) * constants$Elev) * R_a
                        vs_Tmax <- 0.6108 * exp(17.27 * data$Tmax/(data$Tmax +
                                                                               237.3))
                        vs_Tmin <- 0.6108 * exp(17.27 * data$Tmin/(data$Tmin +
                                                                               237.3))
                        vas <- (vs_Tmax + vs_Tmin)/2
                        vabar <- (vs_Tmin * data$RHmax/100 + vs_Tmax * data$RHmin/100)/2
                        B_Mo <- constants$sigma * (0.34 - 0.14 * sqrt(vabar)) *
                                    ((data$Tmax + 273.2)^4 + (data$Tmin + 273.2)^4)/2 *
                                    (1.35 * R_s/R_so - 0.35) * 10^6/86400
                        B_Mo <- aggregate(B_Mo, as.yearmon(data$Date.daily,
                                                           "%m/%y"), FUN = mean)
            }
            if (solar == "sunshine hours") {
                        message1 <- "Sunshine hour data have been used for calculating incoming solar radiation"
            } else
                        if (solar == "cloud") {
                        message1 <- "Cloudiness data have been used for calculating sunshine hour and thus incoming solar radiation"
            } else {
                        message1 <- "Monthly precipitation data have been used for calculating incoming solar radiation"}

            if (Tdew == TRUE) {
                        message6 <- "Data of dew point temperature has been used"
            }else {
                        message6 <- "Data of average vapour pressure has been used to estimate dew point pressure"
            }
            variables <- list(T_Mo = T_Mo, Tdew_Mo = Tdew_Mo, S = S,
                              ptops = ptops, vD_Mo = vD_Mo, v_Mo = v_Mo, deltaMo = deltaMo,
                              G_E = G_E, G_Mo = G_Mo, alpha_Mo = alpha_Mo, B_Mo = B_Mo,
                              message1 = message1, message6 = message6)
            return(variables)
}
