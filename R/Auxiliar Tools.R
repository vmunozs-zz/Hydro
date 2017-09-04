# Auxiliar Tools ----------------------------------------------------------
#'Read a table from excel
#'@export
read.excel <- function(header=TRUE,...) {
            read.table("clipboard",stringsAsFactors = F,sep="\t",header=header,...)
}
#'Write a table from excel
#'@export
write.excel<-function(data.frame,header=TRUE, ...){
            write.table(data.frame, "clipboard", sep="\t", row.names=FALSE, col.names=header)
}

#'Voronoi Polygons
#'@export
voronoipolygons <- function(x) {
            require(deldir)
            require(sp)
            if (.hasSlot(x, 'coords')) {
                        crds <- x@coords
            } else crds <- x
            z <- deldir(crds[,1], crds[,2])
            w <- tile.list(z)
            polys <- vector(mode='list', length=length(w))
            for (i in seq(along=polys)) {
                        pcrds <- cbind(w[[i]]$x, w[[i]]$y)
                        pcrds <- rbind(pcrds, pcrds[1,])
                        polys[[i]] <- Polygons(list(Polygon(pcrds)), ID=as.character(i))
            }
            SP <- SpatialPolygons(polys)
            voronoi <- SpatialPolygonsDataFrame(SP, data=data.frame(x=crds[,1],
                                                                    y=crds[,2], row.names=sapply(slot(SP, 'polygons'),
                                                                                                 function(x) slot(x, 'ID'))))
}

#'Transform a daily zoo in a monthly zoo
#'@param    X        Daily time series in zoo format
#'@param    FUN      Funtion to operation (mean, sum)
#'@param    na.rm    Uses of NA
#'@param    max.miss Maximum amount of missing days in time series to consider a month
#'@return   data frame with outputs
#'@export
daily2monthly<-function(x=flow.daily,FUN=sum, na.rm=TRUE,max.miss=5){

            #            x<-zoo(ECm.st.totp$`3051R4R`$Total.Precip..mm.,ECm.st.totp$`3051R4R`$Date.Time)

            library("lubridate")
            library("hydroTSM")

            months<-as.yearmon(time(x))

            avail.days<-aggregate(x,list(months),FUN = function(x){
                        sum(!is.na(x),na.rm=na.rm)})

            days2months<-aggregate(x,list(months),FUN = FUN,na.rm=na.rm)

            total.days<-days_in_month(months[!duplicated(months)])

            days2months[total.days-avail.days>max.miss]<-NA

            days2months<-zoo(coredata(days2months),as.Date(time(days2months)))
            days2months
}



#'Transform a daily zoo in a monthly zoo
#'@param    X        Daily time series in zoo format
#'@param    FUN      Funtion to operation (mean, sum)
#'@param    na.rm    Uses of NA
#'@param    max.miss Maximum amount of missing days in time series to consider a month
#'@return   data frame with outputs
#'@export
daily2monthly.V2<-function(x=flow.daily,FUN=sum, na.rm=TRUE,max.miss=5){

            #            x<-zoo(ECm.st.totp$`3051R4R`$Total.Precip..mm.,ECm.st.totp$`3051R4R`$Date.Time)

            library("lubridate")
            library("hydroTSM")

            months<-as.yearmon(time(x))

            avail.days<-aggregate(x,list(months),FUN = function(x){
                        sum(!is.na(x),na.rm=na.rm)})

            days2months<-aggregate(x,list(months),FUN = FUN,na.rm=na.rm)

            total.days<-days_in_month(months[!duplicated(months)])

            days2months[total.days-avail.days>max.miss]<-NA

            days2months<-zoo(coredata(days2months),as.Date(time(days2months)))
            days2months
}

#'Transform a zoo time series in a table, available for un-even months
#'@export
zoo2matrix<-function(ts){
            library("lubridate")
            library("zoo")
            #completing gaps
            Range.zoo<-zoo(NA,seq(from = floor_date(time(ts)[1],"year"),
                                  to=ceiling_date(time(ts)[length(ts)],"year")-31,
                                  by="month"))

            Range.zoo.c<-merge(Range.zoo,ts,all = T)
            Range.zoo.c<-Range.zoo.c[,-1]
            c<-Range.zoo.c

            #Prepare matrix
            b<-matrix(data=unlist(c),nrow=nyears(c),ncol=12,byrow=T)
            #define row names
            rownames(b)<-seq(from = year(min(time(c))), to=year(min(time(c)))+nyears(c)-1)
            #define col names
            colnames(b)<-month.abb
            #present matrix
            b

}

#'Estimate the ground elevation base on Google Earth as a input for ECflow.idx
#'@export
ECf.GE.elev<-function(){
            if(exists("ECflow.idx")==TRUE){


                        station.elevation<-data.frame(elevation=mapply(GE.elev,longitude=ECflow.idx$longitude,latitude=ECflow.idx$latitude))

                        station.elevation
            }

            if(exists("NAflow.idx")==TRUE){


                        station.elevation<-data.frame(elevation=mapply(GE.elev,longitude=NAflow.idx$longitude,latitude=NAflow.idx$latitude))

                        station.elevation
            }

            station.elevation
}

#'Distance from the elevation based on Google Earth from a lat and long coordinate
#'
#'Calculate the elevation based on Google Earth
#'@param longitude  Longitude of the point
#'@param latitude   Latitude of the point
#'@export
GE.elev<-function(longitude=30,latitude=40){
            library("RCurl")
            library("rjson")

            key<-"AIzaSyB507449vmoibmLhxfQNiY8Jp-C8lvdpbY" #Key obtained by Victor Muñoz S.
            weblink<-paste("https://maps.googleapis.com/maps/api/elevation/json?locations=",
                           latitude,",",longitude,"&key=",key,
                           sep="")

            location<-getURL(url=weblink,ssl.verifypeer = FALSE)

            x<-fromJSON(location,unexpected.escape = "keep")

            station.elevation<-x$results[[1]]$elevation

            station.elevation
}

#'Distance from the elevation based on Bing from a lat and long coordinate
#'
#'Calculate the elevation based on Bing
#'@param longitude  Longitude of the point
#'@param latitude   Latitude of the point
#'@export
BING.elev<-function(longitude=30,latitude=40){
            library("RCurl")
            library("rjson")

            key<-"AqCUvFDFSjtP1bYIDdyCgzPafBCOSU4mOUmXgW_UdLMBHRn9Ck_-4n4wqlJtIsEQ" #Key obtained by Victor Muñoz S.
            weblink<-paste("http://dev.virtualearth.net/REST/v1/Elevation/List?points=",
                           latitude,",",longitude,"&&key=",key,
                           sep="")

            location<-getURL(url=weblink,ssl.verifypeer = FALSE)

            x<-fromJSON(location,unexpected.escape = "keep")

            station.elevation<-x$resourceSets[[1]]$resources[[1]]$elevations

            station.elevation
}


#'Estimates dew point temperature
#'@param Temp Value of the air temperature in celcius degree
#'@param RH   Relative humidity
#'@return Return the relative humidity
#'@export
RH2DT<-function(Temp,RH){ #Estimate Dew point temperature from Temperature and relative humidity
            Es<-0.611*exp(5423*(1/273-1/(273+Temp)))
            DT<-1/(1/273-0.0001844*log(RH/100*Es/0.611))-273
            DT
}

#'Divide the information from monthly to daily
#'@param timeseries Zoo time series
#'@export
monthly2daily<-function(timeseries){
            #timeseries<-Tmax
            month.time<-seq(as.Date("2001/1/1"), by = "day", length.out = 365)
            time<-zoo(,month.time)
            b<-merge(timeseries,time,all = TRUE)
            b<-na.locf(b)
            b
}

#'Make a sound in the speaker
#'@export
beep <- function(n = 3){
            for(i in seq(n)){
                        system("rundll32 user32.dll,MessageBeep -1")
                        Sys.sleep(.5)
            }
}

#'Distance from the coast from a lat and long coordinate for the ECflow.idx
#'
#'Calculate the distance from the coast for ECflow.idx
#'@export
ECf.coast.dist<-function(){

            library(maps)
            library(maptools)
            library(mapdata)

            if(exists("NAflow.idx")){
                        ECflow.idx<-NAflow.idx
            }


            Global.map <- data.frame(map('worldHires',interior=FALSE,plot=FALSE)[c('x', 'y')])

            ## single point for a simple test

            pts <- as.matrix(ECflow.idx[,c(5,4)])



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

            distance

}

#'Distance from the coast from a lat and long coordinate
#'
#'Calculate the distance from the coast
#'@param longitude  Longitude of the point
#'@param latitude   Latitude of the point
#'@export
coast.dist<-function(longitude=30,latitude=-40){
        library(maps)
        library(sp)
        library(mapdata)

        Global.map <- data.frame(maps::map('worldHires',interior=FALSE,plot = FALSE)[c('x', 'y')])
        xy.coast <- cbind(Global.map$x, Global.map$y)[complete.cases(Global.map$x,Global.map$y), ]
        pts <- as.matrix(c(longitude, latitude))

        round(min(sp::spDistsN1(xy.coast, pts, longlat = TRUE)),0)


}


#'Java Garbage Collector

#'Java Garbage Collector
#'@export
jgc <- function()
{
            .jcall("java/lang/System", method = "gc")
}


#'IDF function – Maximum annual for a define time step

#'IDF function – Maximum annual for a define time step
#'@export
IDF<-function(Pp,n){
            print(n)
            aggregate(xts(c(rep(NA,n-1),rollsum(Pp,k=n)),
                          time(Pp)),
                      by=year,FUN=max,na.rm = T)
}

#' Return Period in text

#' Return Period in text
#'@export
RP.text<-function(Prob){
            sapply(Prob,FUN=function(x){
                        #x<-y[[1]]
                        if(x>=0.5){x.res<-paste0(round(1/(1-x),0)," Wet")}
                        if(x<0.5){x.res<-paste0(round(1/x,0)," Dry")}
                        if(x>0.5&x<0.8){x.res<-paste0(round(1/(1-x),2)," Wet")}

                        x.res
            })

}


#'Function prepare a dataframe to be used in a x,y facet as the functions pairs or hydropairs

#'Function prepare a dataframe to be used in a x,y facet as the functions pairs or hydropairs
#'@export
Pair.Expand <- function(data) {
            grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
            grid <- subset(grid, x != y)
            all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
                        xcol <- grid[i, "x"]
                        ycol <- grid[i, "y"]
                        data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol],
                                   x = data[, xcol], y = data[, ycol], data)
            }))
            all$xvar <- factor(all$xvar, levels = names(data))
            all$yvar <- factor(all$yvar, levels = names(data))
            all
}



#'Function to present the linear relationship expression in a ggplot graph

#'Function to present the linear relationship expression in a ggplot graph
#'@export
stat_smooth_func <- function(mapping = NULL, data = NULL,
                             geom = "smooth", position = "identity",
                             ...,
                             method = "auto",
                             formula = y ~ x,
                             se = TRUE,
                             n = 80,
                             span = 0.75,
                             fullrange = FALSE,
                             level = 0.95,
                             method.args = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             xpos = NULL,
                             ypos = NULL) {
            layer(
                        data = data,
                        mapping = mapping,
                        stat = StatSmoothFunc,
                        geom = geom,
                        position = position,
                        show.legend = show.legend,
                        inherit.aes = inherit.aes,
                        params = list(
                                    method = method,
                                    formula = formula,
                                    se = se,
                                    n = n,
                                    fullrange = fullrange,
                                    level = level,
                                    na.rm = na.rm,
                                    method.args = method.args,
                                    span = span,
                                    xpos = xpos,
                                    ypos = ypos,
                                    ...
                        )
            )
}

library (ggplot2)
#'Function to present the linear relationship expression in a ggplot graph (aux)

#'Function to present the linear relationship expression in a ggplot graph (aux)
#'@export
StatSmoothFunc <- ggproto("StatSmooth", Stat,
                          setup_params = function(data, params) {
                                      # Figure out what type of smoothing to do: loess for small datasets,
                                      # gam with a cubic regression basis for large data
                                      # This is based on the size of the _largest_ group.
                                      if (identical(params$method, "auto")) {
                                                  max_group <- max(table(data$group))

                                                  if (max_group < 1000) {
                                                              params$method <- "loess"
                                                  } else {
                                                              params$method <- "gam"
                                                              params$formula <- y ~ s(x, bs = "cs")
                                                  }
                                      }
                                      if (identical(params$method, "gam")) {
                                                  params$method <- mgcv::gam
                                      }

                                      params
                          },

                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL) {
                                      if (length(unique(data$x)) < 2) {
                                                  # Not enough data to perform fit
                                                  return(data.frame())
                                      }

                                      if (is.null(data$weight)) data$weight <- 1

                                      if (is.null(xseq)) {
                                                  if (is.integer(data$x)) {
                                                              if (fullrange) {
                                                                          xseq <- scales$x$dimension()
                                                              } else {
                                                                          xseq <- sort(unique(data$x))
                                                              }
                                                  } else {
                                                              if (fullrange) {
                                                                          range <- scales$x$dimension()
                                                              } else {
                                                                          range <- range(data$x, na.rm = TRUE)
                                                              }
                                                              xseq <- seq(range[1], range[2], length.out = n)
                                                  }
                                      }
                                      # Special case span because it's the most commonly used model argument
                                      if (identical(method, "loess")) {
                                                  method.args$span <- span
                                      }

                                      if (is.character(method)) method <- match.fun(method)

                                      base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                                      model <- do.call(method, c(base.args, method.args))

                                      m = model
                                      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                                                       list(a = format(coef(m)[1], digits = 3),
                                                            b = format(coef(m)[2], digits = 3),
                                                            r2 = format(summary(m)$r.squared, digits = 3)))
                                      func_string = as.character(as.expression(eq))

                                      if(is.null(xpos)) xpos = min(data$x)*0.9
                                      if(is.null(ypos)) ypos = max(data$y)*0.9
                                      data.frame(x=xpos, y=ypos, label=func_string)

                          },

                          required_aes = c("x", "y")
)


#'Patch hydrological time series

#'Patch hydrological time series
#'@export
Meteo.patch<-function(df=flow.all.zoo, areas,to.be.patched=2,with=4, poly.n=0.75, zero.interception=FALSE,lower.than.zero.is.zero=T,monthly.FUN=mean,units="m3/s",setup=1){
            library(ggplot2)
            library(scales)
            library(copula)
            library(proto)
            library(grid)


            #use of areas for Flows
            if(areas==1){
                        areas<-rep(1,ncol(df))
                        use.areas<-FALSE
            } else{
                        use.areas<-TRUE
            }

            print(paste(names(df)[with],"->",names(df)[to.be.patched]))

            time_ts<-time(df)

            y<-df[,to.be.patched]
            x<-df[,with]

            x_y<-data.frame(x,y)

            x_y<-x_y[complete.cases(x_y),]

            #initial variable
            y2<-y


            #Polynomial regression
            if(poly.n>=1&zero.interception==FALSE){
                        print(paste0("Polynomial expression in ",poly.n," degree"))
                        model<-lm(x_y$y~stats::poly(x_y$x,poly.n,raw=TRUE))

                        y2[is.na(y)]<-polynEval(model$coefficients,x[is.na(y)])

                        #R^2
                        r2<-summary(model)$r.squared

            }

            if(poly.n>=1&zero.interception==TRUE){
                        print(paste0("Polynomial expression in ",poly.n," degree with interception at y=0"))
                        model<-lm(x_y$y~stats::poly(x_y$x,poly.n,raw=TRUE)-1)

                        y2[is.na(y)]<-polynEval(coef = c(0,model$coefficients),x = x[is.na(y)])

                        #R^2
                        r2<-summary(model)$r.squared

            }

            if(poly.n>0&poly.n<1){
                        print(paste0("Loess Fitness with span=",poly.n,". Loess is not affected by zero.interpception parameter"))

                        model<-loess(x_y$y~x_y$x,span=poly.n)
                        #just path, when there is no information
                        y2[is.na(y)]<-predict(model,x[is.na(y)])

                        #R^2
                        ss.dist <- sum(scale(x_y$y, scale=FALSE)^2)
                        ss.resid <- sum(resid(model)^2)

                        r2<-1-ss.resid/ss.dist

            }



            #remove values lower than zero
            if(lower.than.zero.is.zero==T){
                    y2[y2<0]<-0
            }


            #removing extremes ranges -> Interpolation not extrapolation
            library(zoo)

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

            #polynomical evaluation
            if(poly.n>=1&zero.interception==FALSE){
                        reg.df<-data.frame(x=x.predict,
                                           y=polynEval(model$coefficients,x.predict))
            }

            if(poly.n>=1&zero.interception==TRUE){
                        reg.df<-data.frame(x=x.predict,
                                           y=polynEval(c(0,model$coefficients),x.predict))
            }

            #Evaluation of Loess
            if(poly.n>0&poly.n<1){
                        reg.df<-data.frame(x=x.predict,
                                           y=predict(model,x.predict))
            }

            #Monthly graph


            monthly.y2<-daily2monthly.V2(x = y2,FUN = monthly.FUN, na.rm=TRUE,max.miss=5)/areas[to.be.patched]
            monthly.y<-daily2monthly.V2(x = y,FUN = monthly.FUN, na.rm=TRUE,max.miss=5)/areas[to.be.patched]
            monthly.x<-daily2monthly.V2(x = x,FUN = monthly.FUN, na.rm=TRUE,max.miss=5)/areas[with]

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

            units.graph<-paste0("[",units,"]")



            if(use.areas==FALSE){
                        units.graph.uni<-units.graph
            }else{
                        units.graph.uni<-paste0("[",units,"/km2]")
            }

            Fig1<-ggplot(x_y)+
                        geom_point(aes(x=x,y=y),size=0.1)+
                        labs(y=paste(names(df)[to.be.patched],units.graph),
                             x=paste(names(df)[with],units.graph),
                             title="Daily - Patching Relationship\nLinear Scale")+
                        theme(plot.title=element_text(size=16,vjust=2))+
                        geom_path(data=reg.df,aes(x=x,y=y),color="red",size=1)+
                        geom_text(data=r2.df,mapping=aes(x=x,y=y,label=text))

            Fig2<-ggplot(x_y)+
                        geom_point(aes(x=x,y=y),size=0.1)+
                        labs(y=paste(names(df)[to.be.patched],units.graph),
                             x=paste(names(df)[with],units.graph),
                             title="Daily - Patching Relationship\nLogarithmic Scale")+
                        theme(plot.title=element_text(size=16,vjust=2))+
                        geom_path(data=reg.df,aes(x=x,y=y),color="red",size=1)+
                        geom_text(data=r2.df,mapping=aes(x=x,y=y,label=text))+
                        scale_y_log10()+
                        scale_x_log10()+
                        annotation_logticks()


            Fig3<-ggplot(monthly.xy,aes(x=monthly.x,y=monthly.y))+
                        geom_point()+
                        labs(y=paste(names(df)[to.be.patched],units.graph.uni),
                             x=paste(names(df)[with],units.graph.uni),
                             title="Monthly - Original Evaluation")+
                        theme(plot.title=element_text(size=16,vjust=2))+
                        stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)+
                        geom_smooth(method=lm)



            Fig4<-ggplot(monthly.xy2,aes(x=monthly.x,y=monthly.y2))+
                        geom_point()+
                        labs(y=paste(names(df)[to.be.patched],"- PATCHED ",units.graph.uni),
                             x=paste(names(df)[with],units.graph.uni),
                             title="Monthly - Patched Evaluation")+
                        theme(plot.title=element_text(size=16,vjust=2))+
                        stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)+
                        geom_smooth(method=lm)

            Fig5<-ggplot(y2.df)+
                        geom_path(aes(x=Time, y=y2),color="red")+
                        geom_path(aes(x=Time,y=y),color="blue")+
                        labs(y=names(df)[to.be.patched],
                             title=paste(names(df)[to.be.patched],"- Corrected Time Series"))+
                        theme(plot.title=element_text(size=16,vjust=2))

            #time series logarithmitc
            Fig6<-ggplot(y2.df)+
                        geom_path(aes(x=Time, y=y2),color="red")+
                        geom_path(aes(x=Time,y=y),color="blue")+
                        labs(y=names(df)[to.be.patched])+
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
                                      Station=rep(c(names(df)[with],names(df)[to.be.patched]),each = 12),stringsAsFactors = F)


            Fig7<-ggplot(data=monthly.nd.ts)+
                        geom_bar(aes(x=months, y=nd.flow,fill=Station),position=position_dodge(),stat="identity")+
                        scale_x_discrete(breaks=1:12,limits=1:12,labels=month.abb)+
                        scale_y_continuous(label=percent)+
                        labs(y="Non dimensional monthly distribution",
                             title="Monthly Avg. Distribution")+
                        theme(plot.title=element_text(size=16,vjust=2),legend.position="bottom")

            grid.newpage()
            vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

            if(setup==1){
                        pushViewport(viewport(layout = grid.layout(3, 4)))
                        print(Fig1,vp=vplayout(3,1))
                        print(Fig2,vp=vplayout(3,2))
                        print(Fig3,vp=vplayout(3,3))
                        print(Fig4,vp=vplayout(3,4))

                        print(Fig5,vp=vplayout(1,1:3))
                        print(Fig6,vp=vplayout(2,1:3))
                        print(Fig7,vp=vplayout(1:2,4))


            }

            if(setup==2){
                        pushViewport(viewport(layout = grid.layout(4, 3)))
                        print(Fig1,vp=vplayout(3,1))
                        print(Fig2,vp=vplayout(3,2))
                        print(Fig3,vp=vplayout(4,1))
                        print(Fig4,vp=vplayout(4,2))

                        print(Fig5,vp=vplayout(1,1:3))
                        print(Fig6,vp=vplayout(2,1:3))
                        print(Fig7,vp=vplayout(3:4,3))


            }

            results<-list(y,y2[start:end],r2,names(df)[with],names(df)[to.be.patched],Fig1,Fig2,Fig3,Fig4,Fig5,Fig6,Fig7)
            names(results)<-c("original.ts","patched.ts","r2","with","patched","Fig1","Fig2","Fig3","Fig4","Fig5","Fig6","Fig7")

            results
}
