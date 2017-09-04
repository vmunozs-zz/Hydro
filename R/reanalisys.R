#'Reanalisys TS compilation
#'
#'Prepare a Mann-Kendall analysis based on time series.
#'@param path Path for the NETCDF files
#'@param Parameter Parameter based on EC tables
#'@param Longitude Latitude of the site in degrees
#'@param Latitude Longitude of the site in degrees
#'@param Site.Correction Second degree regression C(a,b,c)=> aX^2+bX+c
#'@return return a time series for the parameter and location
#'@export
Reanalysis.ts<-function(path, Parameter, Longitude, Latitude, par.list=NULL,Site.Correction=c(0,1,0)){

        #If there is not define par.list, then the variable parameter.df is fullfilled  the stored data with variable ERA.Interim.2014
        if(is.null(par.list)){
                data(ERA.Interim.2014)
        }else{

                Parameter.df<-par.list
        }

            # #Parameter text
            # Parameter.df<-data.frame(Value=c(11,15,16,61,52,51,2,32,34,33,116,231,236,230,239,234,232,233,237,235,238),
            #                          text=c("Air Temperature - Mean (2m)","Air Temperature - Mean Max (2m)",
            #                                 "Air Temperature - Mean Min (2m)","Precipitation - Total","Humidity - Relative (2m)",
            #                                 "Humidity - Specific (2m)","Sea Level Pressure","Wind Speed - Mean (10m)",
            #                                 "Wind Speed - Meridional (10m)","Wind Speed - Zonal (10m)","Surface Downwelling Shortwave Radiation",
            #                                 "Air Temperature - Extreme Range (2m)","Days - Consecutive Dry","Days - Frost",
            #                                 "Fraction of Time - Annual Precipitation > 95th Percentile",
            #                                 "Fraction of Time - Daily T-min > 90th Percentile","Growing Season Length","Heat Wave Duration Index",
            #                                 "Precipitation - 5 Day Max","Precipitation - Days > 10mm/d",
            #                                 "Precipitation - Simple Daily Intensity Index"),
            #                          var1=NA,var2=NA,FUN=NA,filename1=NA,filename2=NA,filename3=NA,filename4=NA,
            #                          stringsAsFactors = F)
            #
            #
            # #Variable definition for Precipitation
            # Parameter.df$var1[Parameter.df$Value==61]<-"tp"
            # Parameter.df$FUN[Parameter.df$Value==61]<-"sum"
            # Parameter.df$filename1[Parameter.df$Value==61]<-"Total Precipitation - 12  UTC- Step 3-6-9-12.nc"
            #
            # #Variable definition for Temperature
            # Parameter.df$var1[Parameter.df$Value==11]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==11]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==11]<-"2 Metre Temperature - 0 - 0,12.nc"
            #
            # Parameter.df$var2[Parameter.df$Value==11]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==11]<-"mean"
            # Parameter.df$filename2[Parameter.df$Value==11]<-"2 Metre Temperature - 0 - 6,18.nc"
            #
            # #Variable definition for Temperature
            # Parameter.df$var1[Parameter.df$Value==15]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==15]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==15]<-"2 Metre Temperature - 0 - 0,12.nc"
            #
            # Parameter.df$var2[Parameter.df$Value==15]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==15]<-"mean"
            # Parameter.df$filename2[Parameter.df$Value==15]<-"2 Metre Temperature - 0 - 6,18.nc"
            #
            # #Variable definition for Temperature
            # Parameter.df$var1[Parameter.df$Value==16]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==16]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==16]<-"2 Metre Temperature - 0 - 0,12.nc"
            #
            # Parameter.df$var2[Parameter.df$Value==16]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==16]<-"mean"
            # Parameter.df$filename2[Parameter.df$Value==16]<-"2 Metre Temperature - 0 - 6,18.nc"
            #
            #
            # #Variable definition for V wind
            # Parameter.df$var1[Parameter.df$Value==34]<-"v10"
            # Parameter.df$FUN[Parameter.df$Value==34]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==34]<-"10 metre V wind component - 12 UTC.nc"
            #
            # #Variable definition for U wind
            # Parameter.df$var1[Parameter.df$Value==33]<-"u10"
            # Parameter.df$FUN[Parameter.df$Value==33]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==33]<-"10 metre U wind component - 12 UTC.nc"
            #
            # #Variable definition for median wind
            # Parameter.df$var1[Parameter.df$Value==32]<-"v10"
            # Parameter.df$FUN[Parameter.df$Value==32]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==32]<-"10 metre V wind component - 12 UTC.nc"
            #
            # Parameter.df$var2[Parameter.df$Value==32]<-"u10"
            # Parameter.df$filename2[Parameter.df$Value==32]<-"10 metre U wind component - 12 UTC.nc"
            #
            # #Variable definition for Temperature
            # Parameter.df$var1[Parameter.df$Value==232]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==232]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==232]<-"2 Metre Temperature - 0 - 0,12.nc"
            #
            # Parameter.df$var2[Parameter.df$Value==232]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==232]<-"mean"
            # Parameter.df$filename2[Parameter.df$Value==232]<-"2 Metre Temperature - 0 - 6,18.nc"
            #
            # #Variable definition for Temperature
            # Parameter.df$var1[Parameter.df$Value==230]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==230]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==230]<-"2 Metre Temperature - 0 - 0,12.nc"
            #
            # Parameter.df$var2[Parameter.df$Value==230]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==230]<-"mean"
            # Parameter.df$filename2[Parameter.df$Value==230]<-"2 Metre Temperature - 0 - 6,18.nc"
            #
            # #Variable definition for Temperature
            # Parameter.df$var1[Parameter.df$Value==231]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==231]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==231]<-"2 Metre Temperature - 0 - 0,12.nc"
            #
            # Parameter.df$var2[Parameter.df$Value==231]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==231]<-"mean"
            # Parameter.df$filename2[Parameter.df$Value==231]<-"2 Metre Temperature - 0 - 6,18.nc"
            #
            # #Variable definition for Temperature
            # Parameter.df$var1[Parameter.df$Value==232]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==232]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==232]<-"2 Metre Temperature - 0 - 0,12.nc"
            #
            # Parameter.df$var2[Parameter.df$Value==232]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==232]<-"mean"
            # Parameter.df$filename2[Parameter.df$Value==232]<-"2 Metre Temperature - 0 - 6,18.nc"
            #
            # #Variable definition for Temperature
            # Parameter.df$var1[Parameter.df$Value==233]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==233]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==233]<-"2 Metre Temperature - 0 - 0,12.nc"
            #
            # Parameter.df$var2[Parameter.df$Value==233]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==233]<-"mean"
            # Parameter.df$filename2[Parameter.df$Value==233]<-"2 Metre Temperature - 0 - 6,18.nc"
            #
            # #Variable definition for Temperature
            # Parameter.df$var1[Parameter.df$Value==234]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==234]<-"mean"
            # Parameter.df$filename1[Parameter.df$Value==234]<-"2 Metre Temperature - 0 - 0,12.nc"
            #
            # Parameter.df$var2[Parameter.df$Value==234]<-"t2m"
            # Parameter.df$FUN[Parameter.df$Value==234]<-"mean"
            # Parameter.df$filename2[Parameter.df$Value==234]<-"2 Metre Temperature - 0 - 6,18.nc"
            #
            # #Variable definition for precipitation based parameters
            # Parameter.df$var1[Parameter.df$Value==235]<-"tp"
            # Parameter.df$FUN[Parameter.df$Value==235]<-"sum"
            # Parameter.df$filename1[Parameter.df$Value==235]<-"Total Precipitation - 12  UTC- Step 3-6-9-12.nc"
            #
            # Parameter.df$var1[Parameter.df$Value==236]<-"tp"
            # Parameter.df$FUN[Parameter.df$Value==236]<-"sum"
            # Parameter.df$filename1[Parameter.df$Value==236]<-"Total Precipitation - 12  UTC- Step 3-6-9-12.nc"
            #
            # Parameter.df$var1[Parameter.df$Value==237]<-"tp"
            # Parameter.df$FUN[Parameter.df$Value==237]<-"sum"
            # Parameter.df$filename1[Parameter.df$Value==237]<-"Total Precipitation - 12  UTC- Step 3-6-9-12.nc"
            #
            # Parameter.df$var1[Parameter.df$Value==238]<-"tp"
            # Parameter.df$FUN[Parameter.df$Value==238]<-"sum"
            # Parameter.df$filename1[Parameter.df$Value==238]<-"Total Precipitation - 12  UTC- Step 3-6-9-12.nc"
            #
            # Parameter.df$var1[Parameter.df$Value==239]<-"tp"
            # Parameter.df$FUN[Parameter.df$Value==239]<-"sum"
            # Parameter.df$filename1[Parameter.df$Value==239]<-"Total Precipitation - 12  UTC- Step 3-6-9-12.nc"

            #Era Land
            # path2<-"C:/Users/vmunoz/Documents/Climate Change/Era-Land"
            # filename1<-"Total Precipitation - 24 - netcdf-atls99-a562cefde8a29a7288fa0b8b7f9413f7-HPi9io.nc"

            var1<-Parameter.df$var1[Parameter.df$Value==Parameter]
            var2<-Parameter.df$var2[Parameter.df$Value==Parameter]

            FUN<-Parameter.df$FUN[Parameter.df$Value==Parameter]
            filename1<-Parameter.df$filename1[Parameter.df$Value==Parameter]
            filename2<-Parameter.df$filename2[Parameter.df$Value==Parameter]
            filename3<-Parameter.df$filename3[Parameter.df$Value==Parameter]
            filename4<-Parameter.df$filename4[Parameter.df$Value==Parameter]

            Parameter.df<<-Parameter.df

            #NC open
            if(is.na(filename1)){
                        a1<-NA
                        a1.annual<-0
                        a1.monthly<-0
                        a1.daily<-0
            }else{
                        a1<-nc.open.daily(path, FUN, filename = filename1, var1, Latitude, Longitude )
                        a1<<-a1
                        a1.annual<-a1$yearly.timeseries
                        a1.monthly<-a1$monthly.timeseries
                        a1.daily<-a1$daily.timeseries
            }

            if(is.na(filename2)){
                        a2<-NA
                        a2.annual<-0
                        a2.monthly<-0
                        a2.daily<-0
            }else{
                        a2<-nc.open.daily(path, FUN, filename = filename2, var2, Latitude, Longitude )
                        a2<<-a2
                        a2.annual<-a2$yearly.timeseries
                        a2.monthly<-a2$monthly.timeseries
                        a2.daily<-a2$daily.timeseries
            }

            if(is.na(filename3)){
                        a3<-NA
                        a3.annual<-0
                        a3.monthly<-0
                        a3.daily<-0
            }else{
                        a3<-nc.open.daily(path, FUN, filename = filename3, var1, Latitude, Longitude )
                        a3<<-a3
                        a3.annual<-a3$yearly.timeseries
                        a3.monthly<-a3$monthly.timeseries
                        a3.daily<-a3$daily.timeseries
            }

            if(is.na(filename4)){
                        a4<-NA
                        a4.annual<-0
                        a4.monthly<-0
                        a4.daily<-0

            }else{
                        a4<-nc.open.daily(path, FUN, filename = filename4, var1, Latitude, Longitude )
                        a4<<-a4
                        a4.annual<-a4$yearly.timeseries
                        a4.monthly<-a4$monthly.timeseries
                        a4.daily<-a4$daily.timeseries
            }

            if (var1!="t2m"){
                        if(sum(Site.Correction==c(0,1,0))==3){
                                    print("No site corrections")
                                    daily.ts<-a1.daily+a2.daily+a3.daily+a4.daily
                                    monthly.ts<-a1.monthly+a2.monthly+a3.monthly+a4.monthly
                                    annual.ts<-a1.annual+a2.annual+a3.annual+a4.annual

                        }else{
                                    print("Corrected based on site information.\nVector:",Site.Correction)

                                    coredata(daily.ts)<-Site.Correction[1]*coredata(daily.ts)^2+Site.Correction[2]*coredata(daily.ts)+Site.Correction[3]
                                    monthly.ts<-daily2monthly(daily.ts,FUN = FUN)
                                    annual.ts<-daily2annual(daily.ts,FUN=FUN)

                        }


            } else{ #Temperature includes sub-daily!
                        subdaily.ts<-merge(a1$subdaily.timeseries,a2$subdaily.timeseries,all = T)
                        subdaily.ts<-pmax(subdaily.ts[,1],subdaily.ts[,2],na.rm = T)

                        #Time zone correction from merge issue
                        time(subdaily.ts)<-with_tz(time(subdaily.ts),tz="UTC")

                        #Transformation in Kelvin Degrees
                        coredata(subdaily.ts)<-Site.Correction[1]*coredata(subdaily.ts)^2+
                                    Site.Correction[2]*coredata(subdaily.ts)+Site.Correction[3]


                        if(sum(Site.Correction==c(0,1,0))==3){
                                    print("No site corrections")
                        }else{
                                    print(paste("Corrected based on site information. A:",round(Site.Correction[1],3),
                                                " B:",round(Site.Correction[2],3)," C:",round(Site.Correction[3],3),sep=""))
                        }

                        daily.ts<-subdaily2daily.zoo(subdaily.ts,FUN = FUN)
                        monthly.ts<-daily2monthly(daily.ts,FUN = FUN)
                        annual.ts<-daily2annual(daily.ts,FUN=FUN)

                        #Definition of max, min and average daily temperature
                        daily.t.max.ts<-subdaily2daily(subdaily.ts-273.15,FUN = max,na.rm = T)
                        daily.t.min.ts<-subdaily2daily(subdaily.ts-273.15,FUN = min,na.rm = T)
                        daily.t.avg.ts<-subdaily2daily(subdaily.ts-273.15,FUN = mean,na.rm = T)

                        annual.ts<-zoo(coredata(annual.ts),year(time(annual.ts)))
                        #just define a year when more than 360 are defined
                        annual.ts<-annual.ts[as.vector(dwi(daily.ts,out.unit = "years")>360),]

            }

            monthly.ts<-monthly.ts[complete.cases(monthly.ts)]

            if(var1=="tp"){annual.ts<-annual.ts*1000
            monthly.ts<-monthly.ts*1000
            daily.ts<-daily.ts*1000
            units<-"mm"}

            if(var1=="t2m"){annual.ts<-annual.ts-273.15
            monthly.ts<-monthly.ts-273.15
            daily.ts<-daily.ts-273.15
            subdaily.ts<-subdaily.ts-273.15

            units<-"degC"}

            if(var1=="v10"|var1=="u10"){
                        units<-"m/s"}

            #Wind Speed - Mean (10m)
            if(Parameter==32){
                        daily.ts<-sqrt(a1.daily^2+a2.daily^2)
                        monthly.ts<-daily2monthly(x = daily.ts,FUN = mean)
                        annual.ts<-daily2annual(x = daily.ts,FUN=mean)
                        annual.ts<-zoo(annual.ts,year(annual.ts))

                        #Remove actual year
                        annual.ts<-annual.ts[-length(annual.ts),]
            }

            #MAX TEMPERATURE
            if(Parameter==15){
                        daily.ts<-daily.t.max.ts
                        monthly.ts<-daily2monthly(x = daily.ts,FUN = mean)
                        annual.ts<-daily2annual(x = daily.ts,FUN=mean)

                        #Annual Correction
                        annual.ts<-zoo(coredata(annual.ts),year(time(annual.ts)))
                        #just define a year when more than 360 are defined
                        avail.zoo<-aggregate(daily.ts,by=year,FUN=function(x){
                                    sum(!is.na(x))>360
                        })
                        avail.df<-data.frame(date=time(avail.zoo),avail=coredata(avail.zoo))
                        avail.df<-avail.df[avail.df$avail==TRUE,];avail.df<-avail.df$date
                        avail<-complete.cases(match(time(annual.ts),avail.df))

                        annual.ts<-annual.ts[avail]

                        print("Annual, Monthly and Daily values corrected to presented Max Temperature")


            }
            ###MIN TEMPERATURE
            if(Parameter==16){
                        daily.ts<-daily.t.min.ts
                        monthly.ts<-daily2monthly(x = daily.ts,FUN = mean)
                        annual.ts<-daily2annual(x = daily.ts,FUN=mean)

                        #Annual Correction
                        annual.ts<-zoo(coredata(annual.ts),year(time(annual.ts)))
                        #just define a year when more than 360 are defined
                        avail.zoo<-aggregate(daily.ts,by=year,FUN=function(x){
                                    sum(!is.na(x))>360
                        })
                        avail.df<-data.frame(date=time(avail.zoo),avail=coredata(avail.zoo))
                        avail.df<-avail.df[avail.df$avail==TRUE,];avail.df<-avail.df$date
                        avail<-complete.cases(match(time(annual.ts),avail.df))

                        annual.ts<-annual.ts[avail]

                        print("Annual, Monthly and Daily values corrected to presented Min Temperature")
            }

            ####FROST DAYS
            if(Parameter==230){

                        freeze<-daily.t.min.ts
                        freeze[daily.t.min.ts<= 0]<-1
                        freeze[daily.t.min.ts > 0]<-0

                        daily.ts<-freeze
                        monthly.ts<-daily2monthly(x = daily.ts,FUN = sum)
                        annual.ts<-daily2annual(x = daily.ts,FUN=sum)
                        #Remove actual year
                        #Annual Correction
                        annual.ts<-zoo(coredata(annual.ts),year(time(annual.ts)))
                        #just define a year when more than 360 are defined
                        avail.zoo<-aggregate(daily.ts,by=year,FUN=function(x){
                                    sum(!is.na(x))>360
                        })
                        avail.df<-data.frame(date=time(avail.zoo),avail=coredata(avail.zoo))
                        avail.df<-avail.df[avail.df$avail==TRUE,];avail.df<-avail.df$date
                        avail<-complete.cases(match(time(annual.ts),avail.df))

                        annual.ts<-annual.ts[avail]
                        units<-"days"
                        print("Annual, Monthly and Daily values corrected to presented Frost Day")

            }

            ####AIR TEMPERATURE - EXTREME RANGE
            if(Parameter==231){

                        annual.ts.max<-daily2annual(x = daily.t.max.ts,FUN=max)
                        annual.ts.min<-daily2annual(x = daily.t.min.ts,FUN=min)

                        annual.ts<-annual.ts.max-annual.ts.min

                        #Annual Correction
                        annual.ts<-zoo(coredata(annual.ts),year(time(annual.ts)))
                        #just define a year when more than 360 are defined
                        avail.zoo<-aggregate(daily.ts,by=year,FUN=function(x){
                                    sum(!is.na(x))>360
                        })
                        avail.df<-data.frame(date=time(avail.zoo),avail=coredata(avail.zoo))
                        avail.df<-avail.df[avail.df$avail==TRUE,];avail.df<-avail.df$date
                        avail<-complete.cases(match(time(annual.ts),avail.df))

                        annual.ts<-annual.ts[avail]

                        print("Annual values corrected to presented Air Temperature - Extreme Range")

            }

            ####GROWING SEASON LENGTH
            if(Parameter==232){

                        growing<-daily.ts

                        growing[daily.ts>5]<-1
                        growing[daily.ts<=5]<-0

                        growing.z<-zoo(unlist(lapply(rle(coredata(growing))$lengths, seq_len)),time(growing))

                        growing.z[daily.ts<=5]<-0

                        growing.z.max<-daily2annual(x = growing.z,FUN=max)

                        annual.ts<-growing.z.max-10 #5 days at each side

                        units<-"days"
                        #Annual Correction
                        annual.ts<-zoo(coredata(annual.ts),year(time(annual.ts)))
                        #just define a year when more than 360 are defined

                        avail.zoo<-aggregate(daily.ts,by=year,FUN=function(x){
                                    sum(!is.na(x))>360
                        })
                        avail.df<-data.frame(date=time(avail.zoo),avail=coredata(avail.zoo))
                        avail.df<-avail.df[avail.df$avail==TRUE,];avail.df<-avail.df$date
                        avail<-complete.cases(match(time(annual.ts),avail.df))

                        annual.ts<-annual.ts[avail]

                        print("Annual values corrected to presented for Growing season length")

            }

            ####HEAT WAVE DURATION INDEX
            if(Parameter==233){

                        Base.hw<-daily.t.max.ts

                        Base.hw<-daily.t.max.ts[year(time(daily.t.max.ts))>=1979&year(time(daily.t.max.ts))<2006]

                        #mean max temperatue + 5 degC.
                        mean.base.hw<-mean(Base.hw)+5


                        hw<-daily.t.max.ts

                        hw[daily.t.max.ts> mean.base.hw]<-1
                        hw[daily.t.max.ts<= mean.base.hw]<-0

                        hw.z<-zoo(unlist(lapply(rle(coredata(hw))$lengths, seq_len)),time(hw))

                        hw.z[daily.t.max.ts< mean.base.hw]<-0

                        hw.z.max<-daily2annual(x = hw.z,FUN=max)

                        annual.ts<-hw.z.max-5 #5 days at one side

                        units<-"days"
                        #Annual Correction
                        annual.ts<-zoo(coredata(annual.ts),year(time(annual.ts)))

                        #just define a year when more than 360 are defined
                        avail.zoo<-aggregate(daily.ts,by=year,FUN=function(x){
                                    sum(!is.na(x))>360
                        })
                        avail.df<-data.frame(date=time(avail.zoo),avail=coredata(avail.zoo))
                        avail.df<-avail.df[avail.df$avail==TRUE,];avail.df<-avail.df$date
                        avail<-complete.cases(match(time(annual.ts),avail.df))

                        annual.ts<-annual.ts[avail]

                        print("Annual values corrected to presented for Growing season length")

            }

            ####FRACTION OF TIME - Daily T-min > 90th Percentile
            if(Parameter==234){

                        Base.tmin<-daily.t.min.ts

                        Base.tmin<-daily.t.min.ts[year(time(daily.t.min.ts))>=1979&year(time(daily.t.min.ts))<2006]

                        #mean base 90th percentile
                        mean.base.tmin<-as.numeric(quantile(Base.tmin,0.90))


                        tmin<-daily.t.min.ts

                        tmin[daily.t.min.ts> mean.base.tmin]<-1
                        tmin[daily.t.min.ts<= mean.base.tmin]<-0

                        annual.ts<-daily2annual(x = tmin,FUN=sum)

                        ndays<-rep(365,length(year(annual.ts)))

                        ndays[leap_year(year(annual.ts))==TRUE]<-366

                        annual.ts<-100*annual.ts/ndays

                        units<-"%"
                        #Annual Correction
                        annual.ts<-zoo(coredata(annual.ts),year(time(annual.ts)))

                        #just define a year when more than 360 are defined
                        avail.zoo<-aggregate(daily.ts,by=year,FUN=function(x){
                                    sum(!is.na(x))>360
                        })
                        avail.df<-data.frame(date=time(avail.zoo),avail=coredata(avail.zoo))
                        avail.df<-avail.df[avail.df$avail==TRUE,];avail.df<-avail.df$date
                        avail<-complete.cases(match(time(annual.ts),avail.df))

                        annual.ts<-annual.ts[avail]

                        print("Annual values corrected to presented for Fraction of Time - Daily T-min> 90th Percentile")

            }


            ####Pp days>10 mm/day
            if(Parameter==235){

                        X10daymax<-daily.ts[daily.ts>=10]
                        X10daymax.annual.1<-table(year(X10daymax))
                        X10daymax.annual.2<-zoo(as.numeric(X10daymax.annual.1),as.numeric(names(X10daymax.annual.1)))
                        annual.ts<-X10daymax.annual.2

                        print("Annual values corrected to presented >10 mm/day")
                        units<-"days"

                        #Remove actual year
                        annual.ts<-annual.ts[-length(annual.ts),]

            }
            #####Pp days<1 mm/day
            if(Parameter==236){

                        daily.temp<-daily.ts
                        daily.temp[daily.ts<1]<-1
                        daily.temp[daily.ts>=1]<-0
                        x<-coredata(daily.temp)
                        daily.temp<-zoo((!x) * unlist(lapply(rle(x)$lengths, seq_len)),time(daily.temp))
                        annual.ts<-daily2annual(daily.ts,FUN=max,na.rm=TRUE)
                        annual.ts<-zoo(coredata(annual.ts),year(annual.ts))

                        print("Annual values corrected to presented Days - Consecutive Dry")
                        units<-"days"

                        #Remove actual year
                        annual.ts<-annual.ts[-length(annual.ts),]

            }



            ############Pp 5 day max
            if(Parameter==237){

                        X5day<-rollapply(data=daily.ts,width=5,FUN=sum,fill=NA,partial=TRUE, align="center")
                        X5day.annual<-daily2annual(X5day,FUN=max,na.rm=TRUE)
                        annual.ts<-zoo(X5day.annual,year(X5day.annual))
                        print("Annual values corrected to presented 5 Day Max")
                        units<-"mm"

                        #Remove actual year
                        avail.zoo<-aggregate(daily.ts,by=year,FUN=function(x){
                                    sum(!is.na(x))>360
                        })
                        avail.df<-data.frame(date=time(avail.zoo),avail=coredata(avail.zoo))
                        avail.df<-avail.df[avail.df$avail==TRUE,];avail.df<-avail.df$date
                        avail<-complete.cases(match(time(annual.ts),avail.df))

                        annual.ts<-annual.ts[avail]

            }

            #####SIMPLE DAILY INTENSITY INDEX#####
            if(Parameter==238){
                        #Pp days>1 mm/day
                        X1daymax<-daily.ts[daily.ts>=1]
                        X1daymax.annual.1<-table(year(X1daymax))
                        X1daymax.annual.2<-zoo(as.numeric(X1daymax.annual.1),as.numeric(names(X1daymax.annual.1)))

                        #Simple daily intendex
                        SDI.annual<-annual.ts/X1daymax.annual.2
                        annual.ts<-SDI.annual
                        print("Annual values corrected to presented Simple Daily Intensity")
                        units<-"mm/day"

                        #Remove actual year
                        annual.ts<-annual.ts[-length(annual.ts),]
            }


            #####Fraction of time - Annual Precipitation>95th Percentile#####
            if(Parameter==239){

                        Base.p<-daily.ts

                        Base.p<-daily.ts[year(time(daily.ts))>=1979&year(time(daily.ts))<2006]

                        #quantile definition
                        perc.95<-as.numeric(quantile(Base.p,0.95))

                        Analisys.p<-daily.ts
                        #Analisys.p<-daily.ts[year(time(daily.ts))>2005&year(time(daily.ts))<2050]

                        #plot(Analisys.p)
                        P95<-Analisys.p[Analisys.p>perc.95]
                        P95[1:length(P95)]<-1

                        P95<-daily2annual(P95,FUN = sum)

                        ndays<-rep(365,length(year(P95)))
                        ndays[leap_year(year(P95))==TRUE]<-366
                        annual.ts<-100*P95/ndays

                        #Annual Correction
                        annual.ts<-zoo(coredata(annual.ts),year(time(annual.ts)))

                        #just define a year when more than 360 are defined
                        avail.zoo<-aggregate(daily.ts,by=year,FUN=function(x){
                                    sum(!is.na(x))>360
                        })
                        avail.df<-data.frame(date=time(avail.zoo),avail=coredata(avail.zoo))
                        avail.df<-avail.df[avail.df$avail==TRUE,];avail.df<-avail.df$date
                        avail<-complete.cases(match(time(annual.ts),avail.df))

                        annual.ts<-annual.ts[avail]

                        print("Annual values corrected to presented Fraction of Time - Annual Precipitation>95th Percentile")
                        units<-"%"

            }


            if(var1!="t2m"){
                        results<-list(annual.ts,monthly.ts,daily.ts,
                                      Parameter,Parameter.df$text[Parameter.df$Value==Parameter],units)
                        names(results)<-c("Annual.ts","Monthly.ts","Daily.ts","Parameter","Title","units")
            } else{
                        results<-list(annual.ts,monthly.ts,daily.ts,subdaily.ts,
                                      Parameter,Parameter.df$text[Parameter.df$Value==Parameter],units)
                        names(results)<-c("Annual.ts","Monthly.ts","Daily.ts","Subdaily.ts","Parameter","Title","units")

            }

            return(results)
}


#'Reanalisys TS compilation
#'
#'Prepare a Mann-Kendall analysis based on time series.
#'@param annual.ts Annual time series
#'@param parameter Parameter based on EC tables
#'@return return a figure an analysis of the results in percentage
#'@export

Reanalysis.out<-function(Reanalysis.table,Baseline.start = 1975,Baseline.end = 2005,
                         Analysis.start = 2070,Analysis.end = 2099,legend="down"){

            #libraries
            library("ggplot2")
            library("gridExtra")

            annual.ts<-Reanalysis.table$Annual.ts

            Reanalysis.units<-Reanalysis.table$units

            Fig.text<-paste(Reanalysis.table$Title," [",Reanalysis.table$units,"]",sep="")

            annual.df<-data.frame(year=time(annual.ts),data=coredata(annual.ts))

            #Define the amount of information
            year2005<-sum(time(annual.ts)<2005)

            #mann-kendall expressions
            MK.results.std<-MannKendall.std(annual.ts)

            #If the sample information is smaller than 10 years then statistical significance is overwrited to NA
            if(length(annual.ts)<10){
                        MK.results.std$Significance<-NA

            }

            #Change of temperature and MK evaluation
            if(Reanalysis.units=="degC"){annual.ts.c<- 273.15}else{annual.ts.c<-0}

            #MK.results.eval is in kelvin degrees if temperature is evaluated
            MK.results.eval<-MannKendall.eval(ts = annual.ts+annual.ts.c,prob = 0.95,Baseline.start,Baseline.end,Analysis.start,Analysis.end)

            #Expression functions
            LR<-function(x){(MK.results.std$slope[1]*x+MK.results.std$intercept[1])}
            QUANTILE<-function(x){(MK.results.std$slope[2]*x+MK.results.std$intercept[2])}
            MK_TS1<-function(x){(MK.results.std$slope[3]*x+MK.results.std$intercept[3])}
            MK_TS2<-function(x){(MK.results.std$slope[4]*x+MK.results.std$intercept[4])}
            MK_TS3<-function(x){(MK.results.std$slope[5]*x+MK.results.std$intercept[5])}



            point.seq<-if(year2005>0){
                        seq(from = Baseline.start,to = Analysis.end,length.out = 10)
            } else{
                        seq(from = min(time(annual.ts)),to = Analysis.end,length.out = 10)

            }
            #Data Frame
            Regressions.df<-data.frame(Regression=rep(MK.results.std$Method,each=10),
                                       year=rep(point.seq,times = 5),
                                       values=c(LR(point.seq),
                                                QUANTILE(point.seq),
                                                MK_TS1(point.seq),
                                                MK_TS2(point.seq),
                                                MK_TS3(point.seq)))



            #         Table
            Decade.Results.df<-data.frame(Method=sub("\n"," ",MK.results.eval$Method),
                                          "Significance"=100*round(MK.results.std$Significance,3),
                                          "Decade"=paste(signif(MK.results.std$slope*10,2)," ",Reanalysis.units,"/10yr",sep=""),
                                          "Baseline"=signif(MK.results.eval$Evaluation,2))

            names(Decade.Results.df)<-c("Regression Method","Stat.\nSign.\n[%]","Decade\nChange", "Change with\nrespect to\nBaseline[%]")



            #Reanalysis set
            Reanalysis.total<-mean(annual.ts)
            Reanalysis.end<-time(annual.ts)[length(annual.ts)]
            Reanalysis.end<<-Reanalysis.end
            Reanalysis.total<<-Reanalysis.total
            Reanalysis.units<<-Reanalysis.units



            #Differentia the figure based on the amount of information available
            if (year2005>0){

                        Reanalysis.2005<-mean(annual.ts[1:which(time(annual.ts)==2005)])
                        Reanalysis.2005<<-Reanalysis.2005


                        mean.df1<-data.frame(year=1979:Reanalysis.end,
                                             mean.total=Reanalysis.total)
                        mean.df2<-data.frame(year=1979:2005,mean.2005=Reanalysis.2005)


                        #Table Ranges
                        x.range<-Analysis.end-Baseline.start
                        x.min<-Baseline.start+x.range*0.5
                        x.max<-Baseline.start+x.range*0.95

                        y.range<-max(Regressions.df$values,coredata(annual.ts),0)-min(Regressions.df$values,coredata(annual.ts),0)

                        if(legend=="down"){
                                    y.min<-min(Regressions.df$values,coredata(annual.ts),0)+y.range*0.02
                                    y.max<-min(Regressions.df$values,coredata(annual.ts),0)+y.range*0.35
                        } else{
                                    y.min<-min(Regressions.df$values,coredata(annual.ts),0)+y.range*0.6
                                    y.max<-min(Regressions.df$values,coredata(annual.ts),0)+y.range*0.95

                        }

                        mytheme <- gridExtra::ttheme_default(
                                    core = list(fg_params=list(cex = 0.7)),
                                    colhead = list(fg_params=list(cex = 0.7)),
                                    rowhead = list(fg_params=list(cex = 0.7)))

                        #Plot
                        Fig3<-ggplot(annual.df)+
                                    geom_bar(data=annual.df, aes(x=year,y=data), stat="identity",colour="grey")+
                                    geom_line(data=Regressions.df, aes(x=year,y=values,linetype=Regression),size=1,alpha=0.8)+
                                    geom_point(data=Regressions.df, aes(x=year,y=values,shape=Regression),size=4,fill="white")+
                                    scale_shape_manual(values=c(0,1,2,5,6))+
                                    labs(y=Fig.text)+
                                    scale_x_continuous(breaks= seq(from = floor(Baseline.start/10)*10,to = ceiling(Analysis.end/10)*10,by = 20))+
                                    theme(legend.position="bottom")+
                                    annotation_custom(tableGrob(Decade.Results.df,theme=mytheme),
                                                      xmin=x.min,xmax=x.max,ymin=y.min,ymax=y.max)+
                                    geom_line(data=mean.df1,aes(x=year, y=mean.total,
                                                                color=paste("Mean of period 1979 to ",Reanalysis.end,": ",
                                                                            signif(Reanalysis.total,3)," ",Reanalysis.units,sep="")),
                                              size=1.5)+
                                    geom_line(data=mean.df2,aes(x=year, y=mean.2005,
                                                                color=paste("Mean of period 1979 to 2005: ",
                                                                            signif(Reanalysis.2005,3)," ",Reanalysis.units,sep="")),
                                              size=1.5)+
                                    scale_color_discrete(name="ERA-Interim")+
                                guides(shape = guide_legend(ncol = 2))+
                                guides(col = guide_legend(nrow = 2))

            } else{



                        mean.df1<-data.frame(year=min(time(annual.ts)):Reanalysis.end,
                                             mean.total=Reanalysis.total)

                        #Table Ranges
                        x.range<-Analysis.end-Baseline.start
                        x.min<-Baseline.start+x.range*0.5
                        x.max<-Baseline.start+x.range*0.95

                        y.range<-max(Regressions.df$values,coredata(annual.ts),0)-min(Regressions.df$values,coredata(annual.ts),0)

                        if(legend=="down"){
                                    y.min<-min(Regressions.df$values,coredata(annual.ts),0)+y.range*0.02
                                    y.max<-min(Regressions.df$values,coredata(annual.ts),0)+y.range*0.35
                        } else{
                                    y.min<-min(Regressions.df$values,coredata(annual.ts),0)+y.range*0.6
                                    y.max<-min(Regressions.df$values,coredata(annual.ts),0)+y.range*0.95

                        }

                        mytheme <- gridExtra::ttheme_default(
                                    core = list(fg_params=list(cex = 0.7)),
                                    colhead = list(fg_params=list(cex = 0.7)),
                                    rowhead = list(fg_params=list(cex = 0.7)))

                        #Plot
                        Fig3<-ggplot(annual.df)+
                                    geom_bar(data=annual.df, aes(x=year,y=data), stat="identity",colour="grey")+
                                    geom_line(data=Regressions.df, aes(x=year,y=values,linetype=Regression),size=1,alpha=0.8)+
                                    geom_point(data=Regressions.df, aes(x=year,y=values,shape=Regression),size=4,fill="white")+
                                    scale_shape_manual(values=c(0,1,2,5,6))+
                                    labs(y=Fig.text)+
                                    scale_x_continuous(breaks= seq(from = floor(Baseline.start/10)*10,to = ceiling(Analysis.end/10)*10,by = 20))+
                                    theme(legend.position="bottom")+
                                    annotation_custom(tableGrob(Decade.Results.df,theme=mytheme),
                                                      xmin=x.min,xmax=x.max,ymin=y.min,ymax=y.max)+
                                    geom_line(data=mean.df1,aes(x=year, y=mean.total,
                                                                color=paste("Mean of period 1979 to ",Reanalysis.end,": ",
                                                                            signif(Reanalysis.total,3)," ",Reanalysis.units,sep="")),
                                              size=1.5)+
                                    scale_color_discrete(name="ERA-Interim")+
                                guides(shape = guide_legend(ncol = 2))+
                                guides(col = guide_legend(nrow = 2))

            }

            Fig3<<-Fig3
            Fig3
            results<-list(Fig3, MK.results.eval, MK.results.std, Reanalysis.units)
            names(results)<-c("Fig","MK.results","Regression","units")
            results
}

#' Index table for Reanalysis ERA-Interim 2014
#' @author Victor Munoz
#' @references ECMWF
#' @format Index table for Reanalysis ERA-Interim 2014
#' @name ERA.Interim.2014
#' @usage data(ERA.Interim.2014)
NULL


