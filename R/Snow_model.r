#'Snow model based on EcoHydRology
#'@param    Latitude Site Latitude
#'@param    Longitude Site Longitude
#'@param    dpp Value added to the daily results for Total Precipitation
#'@param    dt Value added to the daily results for Temperature
#'@return   dw Value added to the daily results for Wind Speed
#'@export
snowmelt<-function(Latitude,Longitude,par.list=NULL,dpp=0,dt=0,dw=0){

            library("raster")
            library("rgdal")
            library("hydroTSM")
            library("zoo")
            library("EcoHydRology")

            file.topo<-"E:/Climate Change/Gtopo30/World-GTOPO30_Rev2.bil"
            ###########WORLD DEFINITION OF SLOPE AND ASPECT BASED ON GTOPO 30###########################
            #Capture info from Gtopo30
            elevation<-raster(file.topo)

            #Define the area of interest
            AOI<-c(Longitude-0.5,Longitude+0.5,Latitude-0.5,Latitude+0.5)
            elevation.sel<-crop(elevation,AOI)
            #remove no data
            elevation.sel[elevation.sel[]>=2147483647]<-NA

            #Definition of the parmeters slope and aspect in degrees
            elevation.syn<-terrain(elevation.sel, opt=c("slope","aspect"),unit="degrees")

            #Define a coarser mesh to obtain homogeneous parameters
            elevation.syn2<-aggregate(elevation.syn,fact=2,fun=mean)

            plot(elevation.syn)
            #Identify the cell of interest
            cell<-cellFromXY(object = elevation.syn2,xy = c(Longitude,Latitude))

            #Define the parameters
            geo.site<-raster::extract(elevation.syn,cell)
            geo.site2<-raster::extract(elevation.syn2,cell)
            #Parameter values in radians
            geo.site.rad<-as.data.frame(geo.site/180*pi)

            ###################DEFINITION OF AIR TEMPERATURE, PRECIPITATION AND WIND SPEED##################
            #Air Temperature
            Table.t<-Reanalysis.ts(path=path.reanalysis, Parameter = 11,par.list=par.list,Longitude,Latitude)

            baseline.t<-mean(Table.t$Annual.ts[time(Table.t$Annual.ts)<2005&time(Table.t$Annual.ts)>1975])

            dt.1<-(baseline.t+273.15)*(1+dt)-(baseline.t+273.15)

            subdaily.t<-Table.t$Subdaily.ts+dt.1

            #Temperature Check
            #         year.t<-subdaily2annual(subdaily.t,FUN=mean)[as.vector(dwi(subdaily2daily(subdaily.t,FUN=mean),out.unit = "years")>=365)]
            #         year.t<-zoo(coredata(year.t),year(time(year.t)))
            #
            #         proyected.t<-mean(year.t[time(Table.t$Annual.ts)<2005&time(Table.t$Annual.ts)>1975])
            #
            #         ((proyected.t+273.15)-(baseline.t+273.15))/(baseline.t+273.15)

            #Wind speed
            Table.w<-Reanalysis.ts(path=path.reanalysis, Parameter = 32,par.list=par.list,Longitude,Latitude)

            baseline.w<-mean(Table.w$Annual.ts[time(Table.w$Annual.ts)<2005&time(Table.w$Annual.ts)>1975])

            dw.1<-(baseline.w)*(1+dw)-(baseline.w)

            daily.w<-Table.w$Daily.ts+dw.1

            #Wind Check
            #         year.w<-subdaily2annual(daily.w,FUN=mean)[as.vector(dwi(subdaily2daily(daily.w,FUN=mean),out.unit = "years")>=365)]
            #         year.w<-zoo(coredata(year.w),year(time(year.w)))
            #
            #         proyected.w<-mean(year.w[time(Table.w$Annual.ts)<2005&time(Table.w$Annual.ts)>1975])
            #
            #         (proyected.w-baseline.w)/baseline.w
            #

            #Precipitation
            Table.pp<-Reanalysis.ts(path=path.reanalysis, Parameter = 61,par.list=par.list,Longitude,Latitude)

            baseline.pp<-mean(Table.pp$Annual.ts[time(Table.pp$Annual.ts)<2005&time(Table.pp$Annual.ts)>1975])

            #rainy days=> days with more than 1 mm
            baseline.rain.days<-mean(dwi(Table.pp$Daily.ts[Table.pp$Daily.ts>=1])[as.vector(dwi(Table.pp$Daily.ts)>=365)])

            dpp.1<-((baseline.pp)*(1+dpp)-(baseline.pp))/baseline.rain.days


            #Precipitation difference correction
            optm.pp<-function(dpp.1=dpp.1){
                        #Just rainy days

                        daily.pp<-Table.pp$Daily.ts
                        daily.pp[Table.pp$Daily.ts>=1]<-Table.pp$Daily.ts[Table.pp$Daily.ts>=1]+dpp.1

                        #Precipitation Check
                        year.pp<-daily2annual(daily.pp,FUN=sum)[as.vector(dwi(daily.pp,out.unit = "years")>=365)]
                        year.pp<-zoo(coredata(year.pp),year(time(year.pp)))

                        proyected.pp<-mean(year.pp[time(Table.pp$Annual.ts)<2005&time(Table.pp$Annual.ts)>1975])

                        results.pp<-(proyected.pp-baseline.pp)/baseline.pp

                        error.pp<-(results.pp-dpp)^2

                        error.pp

            }

            #optimization to find the proper correction

            if(dpp==0){
                        dpp.1<-0
            }else{
                        result.pp.2<-optimize(optm.pp,interval=c(-5,5),tol = 1e-10)
                        dpp.1<-result.pp.2$minimum
            }

            daily.pp<-Table.pp$Daily.ts
            daily.pp[Table.pp$Daily.ts>=1]<-Table.pp$Daily.ts[Table.pp$Daily.ts>=1]+dpp.1

            #         #Precipitation Check
            #         year.pp<-daily2annual(daily.pp,FUN=sum)[as.vector(dwi(daily.pp,out.unit = "years")>=365)]
            #         year.pp<-zoo(coredata(year.pp),year(time(year.pp)))
            #
            #         proyected.pp<-mean(year.pp[time(Table.pp$Annual.ts)<2005&time(Table.pp$Annual.ts)>1975])
            #
            #         results.pp<-(proyected.pp-baseline.pp)/baseline.pp
            #         print(results.pp)
            #


            #Definition of max, min and average daily temperature
            Table.t.max<-subdaily2daily(subdaily.t,FUN = max,na.rm = T)
            Table.t.min<-subdaily2daily(subdaily.t,FUN = min,na.rm = T)
            Table.t.avg<-subdaily2daily(subdaily.t,FUN = mean,na.rm = T)

            Table.t<-data.frame(Date=time(Table.t.min),
                                T.min=coredata(Table.t.min),
                                T.avg=coredata(Table.t.avg),
                                T.max=coredata(Table.t.max))

            Table.w<-data.frame(Date=time(daily.w),W.avg=coredata(daily.w))

            Table.pp<-data.frame(Date=time(daily.pp),Pp.avg=coredata(daily.pp))

            Table<-merge(Table.t,Table.w)
            Table<-merge(Table,Table.pp)

            sign.1<-function(x){
                        if(x>=0){"+"}else{"-"}
            }

            cat(paste0("ERA-Interim conditions including:\n",
                       "Tot.Pp: ",sign.1(dpp),100*dpp," % /",sign.1(dpp.1),signif(dpp.1,3),"mm/rainy.day[Pp>=0.1mm])\n",
                       "Temp  : ",sign.1(dt),100*dt,  " % /",sign.1(dt.1),signif(dt.1,3),"degC)\n",
                       "Wind  : ",sign.1(dw),100*dw,  " % /",sign.1(dw.1),signif(dw.1,2),"m/s)\n",
                       "Terrain Slope  :",signif(geo.site[1],3)," deg\n",
                       "Terrain Aspect :",signif(geo.site[2],3)," deg\n"))

            site.snow<-SnowMelt(Date=Table$Date,
                                precip_mm = Table$Pp.avg,
                                Tmax_C = Table$T.max,
                                Tmin_C = Table$T.min,
                                lat_deg = Latitude,
                                slope = geo.site.rad$slope,
                                aspect = geo.site.rad$aspect,
                                tempHt = 1,windHt = 2,windSp = Table$W.avg)

            #RAINFAL ANALYSIS
            rain<-zoo(site.snow$Rain_mm,site.snow$Date)
            rain.m<-daily2monthly.V2(rain,FUN = sum)
            rain.table<-zoo2matrix(rain.m)
            rain.avg<-colMeans(rain.table,na.rm = T)
            rain.annual<-sum(rain.avg)

            rain.list<-list(rain,rain.m,rain.table,rain.avg,rain.annual)
            names(rain.list)<-c("daily.ts","monthly.ts","monthly.table","monthly.avg","annual.avg")

            #SNOWFALL ANALYSIS
            snow<-zoo(site.snow$SnowfallWatEq_mm,site.snow$Date)
            snow.m<-daily2monthly.V2(snow,FUN = sum)
            snow.table<-zoo2matrix(snow.m)
            snow.avg<-colMeans(snow.table,na.rm = T)
            snow.annual<-sum(snow.avg)

            snow.list<-list(snow,snow.m,snow.table,snow.avg,snow.annual)
            names(snow.list)<-c("daily.ts","monthly.ts","monthly.table","monthly.avg","annual.avg")

            #SNOW DEPTH ANALYSIS
            snow.depth<-zoo(site.snow$SnowDepth_m,site.snow$Date)
            snow.depth.m<-daily2monthly.V2(snow.depth,FUN = mean)
            snow.depth.table<-zoo2matrix(snow.depth.m)
            snow.depth.avg<-colMeans(snow.depth.table,na.rm = T)
            snow.depth.annual<-sum(snow.depth.avg*c(31,28.25,31,30,31,30,31,31,30,31,30,31))/365.25

            snow.depth.list<-list(snow.depth,snow.depth.m,snow.depth.table,snow.depth.avg,snow.depth.annual)
            names(snow.depth.list)<-c("daily.ts","monthly.ts","monthly.table","monthly.avg","annual.avg")

            #SNOW WATER EQ ANALYSIS
            snow.waterEQ<-zoo(site.snow$SnowWaterEq_mm,site.snow$Date)
            snow.waterEQ.m<-daily2monthly.V2(snow.waterEQ,FUN = mean)
            snow.waterEQ.table<-zoo2matrix(snow.waterEQ.m)
            snow.waterEQ.avg<-colMeans(snow.waterEQ.table,na.rm = T)
            snow.waterEQ.annual<-sum(snow.waterEQ.avg*c(31,28.25,31,30,31,30,31,31,30,31,30,31))/365.25

            snow.waterEQ.list<-list(snow.waterEQ,snow.waterEQ.m,snow.waterEQ.table,snow.waterEQ.avg,snow.waterEQ.annual)
            names(snow.waterEQ.list)<-c("daily.ts","monthly.ts","monthly.table","monthly.avg","annual.avg")

            #SNOWMELT ANALYSIS
            snow.melt<-zoo(site.snow$SnowMelt_mm,site.snow$Date)
            snow.melt.m<-daily2monthly.V2(snow.melt,FUN = max)
            snow.melt.table<-zoo2matrix(snow.melt.m)
            snow.melt.avg<-colMeans(snow.melt.table,na.rm = T)
            snow.melt.max<-max(snow.melt.avg)

            snow.melt.list<-list(snow.melt,snow.melt.m,snow.melt.table,snow.melt.avg,snow.melt.max)
            names(snow.melt.list)<-c("daily.ts","monthly.max.ts","monthly.max.table","monthly.avg","annual.max.avg")


            results<-list(site.snow,rain.list,snow.list,snow.depth.list,snow.waterEQ.list,snow.melt.list)
            names(results)<-c("model.results","rainfall_mm","snowfall_mm","snow.depth_m","SWE_mm","Max_SnowMelt_mm")
            results
}

#'Potential Solar based on EcoHydRology corrected for higher latitudes
#'
#'Potential Solar based on EcoHydRology corrected for higher latitudes
#'@export
PotentialSolar<-function(lat,Jday){
            # potential solar radiation at the edge of the atmospher [kJ m-2 d-1]

            #lat: latitdue [rad]
            #Jday: Julian date or day of the year [day]

            # solar declination [rad]
            dec<-declination(Jday)

            #Correction for higher latitudes!
            delta<- -tan(dec)*tan(lat)

            delta<-max(min(delta,1),-1)


            117500*(acos(delta)*sin(lat)*sin(dec)+cos(lat)*cos(dec)*sin(acos(-delta)))/pi
}

#'Solar radiation at ground surface from EcoHydRology corrected for higher latitudes
#'
#'Solar radiation at ground surface from EcoHydRology corrected for higher latitudes
#'@export
Solar<-function (lat, Jday, Tx, Tn, albedo=0.2, forest=0, slope=0, aspect = 0, units="kJm2d", latUnits = "unknown", printWarn=TRUE) {

            if ((abs(lat) > pi/2 & latUnits == "unknown") | latUnits == "degrees" ){
                        if (printWarn==TRUE)
                                    lat <- lat*pi/180
            } else if (latUnits == "unknown"){
                        if (printWarn==TRUE) warning("In Solar(): Input latitude units are not specified and assumed to be radians")
            }

            if (units == "kJm2d") convert <- 1 else convert <- 86.4  # can convert to W/m2
            return( signif((1 - albedo) * (1 - forest) * transmissivity(Tx, Tn) *
                                       Hydro::PotentialSolar(lat, Jday) * slopefactor(lat, Jday, slope, aspect) / convert , 5 ))
}

#'SnowMelt model from EcoHydRology corrected for higher latitudes
#'
#'SnowMelt model from EcoHydRology corrected for higher latitudes
#'@export
SnowMelt<-function(Date, precip_mm, Tmax_C, Tmin_C, lat_deg, slope=0, aspect=0, tempHt=1, windHt=2, groundAlbedo=0.25, 		SurfEmissiv=0.95, windSp=2, forest=0, startingSnowDepth_m=0, startingSnowDensity_kg_m3=450){
            ## Constants :
            WaterDens <- 1000			# kg/m3
            lambda <- 3.35*10^5			# latent heat of fusion (kJ/m3)
            lambdaV <- 2500				# (kJ/kg) latent heat of vaporization
            SnowHeatCap <- 2.1			# kJ/kg/C
            LatHeatFreez <- 333.3		# kJ/kg
            Cw <- 4.2*10^3				# Heat Capacity of Water (kJ/m3/C)

            ##	Converted Inputs :
            Tav <- (Tmax_C+Tmin_C)/2		# degrees C
            precip_m <- precip_mm*0.001	 	# precip in m
            R_m <- precip_m					# (m) depth of rain
            R_m[which(Tav < 0)] <- 0		# ASSUMES ALL SNOW at < 0C
            NewSnowDensity <- 50+3.4*(Tav+15)		# kg/m3
            NewSnowDensity[which(NewSnowDensity < 50)] <- 50
            NewSnowWatEq <- precip_m				# m
            NewSnowWatEq[which(Tav >= 0)] <- 0			# No new snow if average temp above or equals 0 C
            NewSnow <- NewSnowWatEq*WaterDens/NewSnowDensity		# m
            JDay <- strptime(Date, format="%Y-%m-%d")$yday+1
            lat <- lat_deg*pi/180		#	latitude in radians
            rh 	<- log((windHt+0.001)/0.001)*log((tempHt+0.0002)/0.0002)/(0.41*0.41*windSp*86400)	# (day/m) Thermal Resistance
            if (length(windSp)==1) rh <- rep(rh,length(precip_mm))									##	creates a vector of rh values
            cloudiness 		<- EstCloudiness(Tmax_C,Tmin_C)
            AE 				<- AtmosphericEmissivity(Tav, cloudiness)	# (-) Atmospheric Emissivity

            #  New Variables	:
            SnowTemp 		<- rep(0,length(precip_m)) 		# Degrees C
            rhos 			<- SatVaporDensity(SnowTemp)	# 	vapor density at surface (kg/m3)
            rhoa 			<- SatVaporDensity(Tmin_C)		#	vapor density of atmoshpere (kg/m3)
            SnowWaterEq 	<- vector(length=length(precip_mm))		#  (m) Equiv depth of water
            TE 				<- rep(SurfEmissiv,length(precip_mm))	#	(-) Terrestrial Emissivity
            DCoef 			<- rep(0,length(precip_mm))				#   Density Coefficient (-) (Simplified version)
            SnowDensity 	<- rep(450,length(precip_mm))			#  (kg/m3)  Max density is 450
            SnowDepth 		<- vector(length=length(precip_mm))		#  (m)
            SnowMelt 		<- rep(0,length(precip_mm))				#  (m)
            Albedo 			<- rep(groundAlbedo,length(precip_mm)) 	#  (-) This will change for days with snow

            ##	Energy Terms
            H 		<- vector(length=length(precip_mm))	#	Sensible Heat exchanged (kJ/m2/d)
            E 		<- vector(length=length(precip_mm))	#	Vapor Energy	(kJ/m2/d)
            S 		<- vector(length=length(precip_mm))	#	Solar Radiation (kJ/m2/d)
            La 		<- Longwave(AE, Tav)					#	Atmospheric Longwave Radiation (kJ/m2/d)
            Lt 		<- vector(length=length(precip_mm))	#	Terrestrial Longwave Radiation (kJ/m2/d)
            G 		<- 173								#	Ground Condution (kJ/m2/d)
            P 		<- Cw * R_m * Tav					# 	Precipitation Heat (kJ/m2/d)
            Energy 	<- vector(length=length(precip_mm))	# Net Energy (kJ/m2/d)

            ##  Initial Values.
            SnowWaterEq[1] 	<- startingSnowDepth_m * startingSnowDensity_kg_m3 / WaterDens
            SnowDepth[1] 	<- startingSnowDepth_m
            Albedo[1] <- ifelse(NewSnow[1] > 0, 0.98-(0.98-0.50)*exp(-4*NewSnow[1]*10),ifelse(startingSnowDepth_m == 0, groundAlbedo, max(groundAlbedo, 0.5+(groundAlbedo-0.85)/10)))  # If snow on the ground or new snow, assume Albedo yesterday was 0.5
            S[1] <- Hydro::Solar(lat=lat,Jday=JDay[1], Tx=Tmax_C[1], Tn=Tmin_C[1], albedo=Albedo[1], forest=forest, aspect=aspect, slope=slope)
            H[1] <- 1.29*(Tav[1]-SnowTemp[1])/rh[1]
            E[1] <- lambdaV*(rhoa[1]-rhos[1])/rh[1]
            if(startingSnowDepth_m>0) TE[1] <- 0.97
            Lt[1] <- Longwave(TE[1],SnowTemp[1])
            Energy[1] <- S[1] + La[1] - Lt[1] + H[1] + E[1] + G + P[1]
            SnowDensity[1] <- ifelse((startingSnowDepth_m+NewSnow[1])>0, min(450, (startingSnowDensity_kg_m3*startingSnowDepth_m + NewSnowDensity[1]*NewSnow[1])/(startingSnowDepth_m+NewSnow[1])), 450)
            SnowMelt[1] <- max(0,	min((startingSnowDepth_m/10+NewSnowWatEq[1]),  # yesterday on ground + today new
                                      (Energy[1]-SnowHeatCap*(startingSnowDepth_m/10+NewSnowWatEq[1])*WaterDens*(0-SnowTemp[1]))/(LatHeatFreez*WaterDens) ) )
            SnowDepth[1] <- max(0,(startingSnowDepth_m/10 + NewSnowWatEq[1]-SnowMelt[1])*WaterDens/SnowDensity[1])
            SnowWaterEq[1] <- max(0,startingSnowDepth_m/10-SnowMelt[1]+NewSnowWatEq[1])



            ##  Snow Melt Loop
            for (i in 2:length(precip_m)){
                        if (NewSnow[i] > 0){
                                    Albedo[i] <- 0.98-(0.98-Albedo[i-1])*exp(-4*NewSnow[i]*10)
                        } else if (SnowDepth[i-1] < 0.1){
                                    Albedo[i] <- max(groundAlbedo, Albedo[i-1]+(groundAlbedo-0.85)/10)
                        } else Albedo[i] <- 0.35-(0.35-0.98)*exp(-1*(0.177+(log((-0.3+0.98)/(Albedo[i-1]-0.3)))^2.16)^0.46)

                        S[i] <- Hydro::Solar(lat=lat,Jday=JDay[i], Tx=Tmax_C[i], Tn=Tmin_C[i], albedo=Albedo[i-1], forest=forest, aspect=aspect, slope=slope, printWarn=FALSE)

                        if(SnowDepth[i-1] > 0) TE[i] <- 0.97 	#	(-) Terrestrial Emissivity
                        if(SnowWaterEq[i-1] > 0 | NewSnowWatEq[i] > 0) {
                                    DCoef[i] <- 6.2
                                    if(SnowMelt[i-1] == 0){
                                                SnowTemp[i] <- max(min(0,Tmin_C[i]),min(0,(SnowTemp[i-1]+min(-SnowTemp[i-1],Energy[i-1]/((SnowDensity[i-1]*
                                                                                                                                                      SnowDepth[i-1]+NewSnow[i]*NewSnowDensity[i])*SnowHeatCap*1000)))))
                                    }
                        }

                        rhos[i] <- SatVaporDensity(SnowTemp[i])
                        H[i] <- 1.29*(Tav[i]-SnowTemp[i])/rh[i]
                        E[i] <- lambdaV*(rhoa[i]-rhos[i])/rh[i]
                        Lt[i] <- Longwave(TE[i],SnowTemp[i])
                        Energy[i] <- S[i] + La[i] - Lt[i] + H[i] + E[i] + G + P[i]

                        if (Energy[i]>0) k <- 2 else k <- 1

                        SnowDensity[i] <- ifelse((SnowDepth[i-1]+NewSnow[i])>0, min(450,
                                                                                    ((SnowDensity[i-1]+k*30*(450-SnowDensity[i-1])*exp(-DCoef[i]))*SnowDepth[i-1] + NewSnowDensity[i]*NewSnow[i])/(SnowDepth[i-1]+NewSnow[i])), 450)

                        SnowMelt[i] <- max(0,	min( (SnowWaterEq[i-1]+NewSnowWatEq[i]),  # yesterday on ground + today new
                                                   (Energy[i]-SnowHeatCap*(SnowWaterEq[i-1]+NewSnowWatEq[i])*WaterDens*(0-SnowTemp[i]))/(LatHeatFreez*WaterDens) )  )

                        SnowDepth[i] <- max(0,(SnowWaterEq[i-1]+NewSnowWatEq[i]-SnowMelt[i])*WaterDens/SnowDensity[i])
                        SnowWaterEq[i] <- max(0,SnowWaterEq[i-1]-SnowMelt[i]+NewSnowWatEq[i])	# (m) Equiv depth of water
            }

            Results<-data.frame(Date, Tmax_C, Tmin_C, precip_mm, R_m*1000, NewSnowWatEq*1000,SnowMelt*1000, NewSnow, SnowDepth, SnowWaterEq*1000)
            colnames(Results)<-c("Date", "MaxT_C", "MinT_C", "Precip_mm", "Rain_mm", "SnowfallWatEq_mm", "SnowMelt_mm", "NewSnow_m", "SnowDepth_m", "SnowWaterEq_mm")
            return(Results)
}

