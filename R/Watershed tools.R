#' Watershed Delineation with Taudem
#'
#' @export
watershed.poly<-function(Longitude,Latitude,Area,filename,highdef=F,AOI.length=0.05){

        library(raster)
        library(rgdal)
        library(shapefiles)
        library(mapplots)
        library(maptools)

        filename<-make.names(filename)

        # Set working directory to your location

        file.topo<-"E:/Climate Change/Gtopo30/World-Gtopo30_World_Cylindrical_Equal_Area.bil"
        file.facc<-"E:/Climate Change/Gtopo30/FlowAcc_World-Gtopo30_World_Cylindrical_Equal_Area.bil"
        if(highdef==T){
                file.topo<-"E:/Climate Change/GMTED2010/NorthAmerica_GMTED_med075-fill.bil"
        }

        ###########WORLD DEFINITION OF SLOPE AND ASPECT BASED ON GTOPO 30###########################
        #Capture info from Gtopo30
        elevation<-raster(file.topo)
        elevation.facc<-raster(file.facc)
        #plot(elevation)
        CRS.longlat<-CRS("+init=epsg:4326")
        CRS.bil<-elevation@crs

        #coordinate

        coords<-cbind(Longitude,Latitude)
        sp<-SpatialPoints(coords,proj4string = CRS.longlat)
        coords.bil<-spTransform(sp,CRS.bil)
        coords.bil@coords

        AOI<-function(Longitude,Latitude,dist){
                CRS.longlat<-CRS("+init=epsg:4326")

                AOI.result<-c(Longitude-dist,Longitude+dist,
                              Latitude-dist,Latitude+dist)
                AOI.coords<-data.frame(Longitude=c(AOI.result[1],AOI.result[2]),Latitude=c(AOI.result[3],AOI.result[4]))
                AOI.sp<-SpatialPoints(AOI.coords,proj4string = CRS.longlat)
                AOI.sp@bbox
                #project(xy = AOI.sp,proj = CRS.bil,inv = F)

                AOI.bil<-spTransform(AOI.sp,CRS.bil)
                AOI.bil@coords
                AOI.bil<-c(AOI.bil@coords[1,1],AOI.bil@coords[2,1],AOI.bil@coords[1,2],AOI.bil@coords[2,2])
                AOI.bil
        }

        #Define the area of interest
        dist<-3*sqrt(Area*2)/111#degrees #
        AOI.res<-AOI(Longitude,Latitude,dist)
        z<-crop(elevation,AOI.res)
        facc<-crop(elevation.facc,AOI(Longitude,Latitude,AOI.length))

        #Area
        plot(z)

        #Adjust to watershed
        plot(facc)
        points(coords.bil@coords[1],coords.bil@coords[2],pch=19,col=2)


        expected<-Area/((739.3401^2)/1e6)

        facc.corr<-raster::calc(x=facc,fun=function(x)sqrt({x-expected}^2))
        plot(facc.corr)

        facc.min<-raster::which.min(facc.corr)[1]

        print(paste("Raster Expected:",round(expected,0)))
        print(paste("Obtained:",raster::extract(facc,facc.min)))

        gauge.location<-xyFromCell(facc.corr,facc.min)

        #if(highdef==T){gauge.location<-c(Longitude,Latitude)}

        writeRaster(z,"logan.tif",format="GTiff", options=c("COMPRESS=NONE","TFW=YES"),overwrite=TRUE)

        # Pitremove

        system("mpiexec -n 8 pitremove -z logan.tif -fel loganfel.tif")
        fel=raster("loganfel.tif")
        plot(fel)


        # D8 flow directions
        system("mpiexec -n 8 D8Flowdir -p loganp.tif -sd8 logansd8.tif -fel loganfel.tif",show.output.on.console=F,invisible=F)
        p=raster("loganp.tif")
        plot(p)
        sd8=raster("logansd8.tif")
        plot(sd8)

        # Contributing area
        system("mpiexec -n 8 AreaD8 -p loganp.tif -ad8 loganad8.tif")
        ad8=raster("loganad8.tif")
        plot(log(ad8))
        # zoom(log(ad8))


        # Grid Network
        system("mpiexec -n 8 Gridnet -p loganp.tif -gord logangord.tif -plen loganplen.tif -tlen logantlen.tif")
        gord=raster("logangord.tif")
        plot(gord)
        # zoom(gord)

        # DInf flow directions
        system("mpiexec -n 8 DinfFlowdir -ang loganang.tif -slp loganslp.tif -fel loganfel.tif",show.output.on.console=F,invisible=F)
        ang=raster("loganang.tif")
        plot(ang)
        slp=raster("loganslp.tif")
        plot(slp)


        # Dinf contributing area
        system("mpiexec -n 8 AreaDinf -ang loganang.tif -sca logansca.tif")
        sca=raster("logansca.tif")
        plot(log(sca))
        # zoom(log(sca))

        # Threshold
        system(paste("mpiexec -n 8 Threshold -ssa loganad8.tif -src logansrc.tif -thresh",expected/10))
        src=raster("logansrc.tif")
        plot(src)
        # zoom(src)

        # a quick R function to write a shapefile

        #Point
        dd <- data.frame(Id=1,X=gauge.location[1],Y=gauge.location[2])
        ddTable <- data.frame(Id=c(1),Name=paste("Outlet",1,sep=""))
        ddShapefile <- convert.to.shapefile(dd, ddTable, "Id", 1)
        write.shapefile(ddShapefile, "ApproxOutlets", arcgis=T)


        # Move Outlets
        system(paste("mpiexec -n 8 moveoutletstostreams -p loganp.tif -src logansrc.tif -o approxoutlets.shp -om Outlet.shp -md ",1000) )
        outpt=read.shp("outlet.shp")
        approxpt=read.shp("ApproxOutlets.shp")

        plot(src)
        points(outpt$shp[2],outpt$shp[3],pch=19,col=2)
        points(approxpt$shp[2],approxpt$shp[3],pch=19,col=4)

        # zoom(src)


        # Contributing area upstream of outlet
        system("mpiexec -n 8 Aread8 -p loganp.tif -o Outlet.shp -ad8 loganssa.tif")
        ssa=raster("loganssa.tif")
        plot(ssa)


        # Threshold
        system(paste("mpiexec -n 8 threshold -ssa loganssa.tif -src logansrc1.tif -thresh ",log(Area),sep=""))
        src1=raster("logansrc1.tif")
        plot(src1)
        # zoom(src1)

        # Stream Reach and Watershed
        system("mpiexec -n 8 Streamnet -fel loganfel.tif -p loganp.tif -ad8 loganad8.tif -src logansrc1.tif -o outlet.shp -ord loganord.tif -tree logantree.txt -coord logancoord.txt -net logannet.shp -w loganw.tif")
        plot(raster("loganord.tif"))
        # zoom(raster("loganord.tif"))
        plot(raster("loganw.tif"))

        # Plot streams using stream order as width
        snet=read.shapefile("logannet")
        ns=length(snet$shp$shp)
        for(i in 1:ns)
        {
                lines(snet$shp$shp[[i]]$points,lwd=snet$dbf$dbf$Order[i])
        }

        # Peuker Douglas stream definition
        system("mpiexec -n 8 PeukerDouglas -fel loganfel.tif -ss loganss.tif")
        ss=raster("loganss.tif")
        plot(ss)
        # zoom(ss)

        #  Accumulating candidate stream source cells
        system("mpiexec -n 8 Aread8 -p loganp.tif -o outlet.shp -ad8 loganssa.tif -wg loganss.tif")
        ssa=raster("loganssa.tif")
        plot(ssa)

        #  Drop Analysis
        system("mpiexec -n 8 Dropanalysis -p loganp.tif -fel loganfel.tif -ad8 loganad8.tif -ssa loganssa.tif -drp logandrp.txt -o outlet.shp -par 5 500 10 0")

        # Deduce that the optimal threshold is 300
        # Stream raster by threshold
        system("mpiexec -n 8 Threshold -ssa loganssa.tif -src logansrc2.tif -thresh 10")
        plot(raster("logansrc2.tif"))

        # Stream network
        system("mpiexec -n 8 Streamnet -fel loganfel.tif -p loganp.tif -ad8 loganad8.tif -src logansrc2.tif -ord loganord2.tif -tree logantree2.dat -coord logancoord2.dat -net logannet2.shp -w loganw2.tif -o Outlet.shp",show.output.on.console=F,invisible=F)

        plot(raster("loganw2.tif"))
        wat<-raster("loganw2.tif")
        snet=read.shapefile("logannet2")
        ns=length(snet$shp$shp)
        for(i in 1:ns)
        {
                lines(snet$shp$shp[[i]]$points,lwd=snet$dbf$dbf$Order[i])
        }

        # Wetness Index
        system("mpiexec -n 8 SlopeAreaRatio -slp loganslp.tif -sca logansca.tif -sar logansar.tif", show.output.on.console=F, invisible=F)
        sar=raster("logansar.tif")
        wi=sar
        wi[,]=-log(sar[,])
        plot(wi)

        # Distance Down
        system("mpiexec -n 8 DinfDistDown -ang loganang.tif -fel loganfel.tif -src logansrc2.tif -m ave v -dd logandd.tif",show.output.on.console=F,invisible=F)
        plot(raster("logandd.tif"))

        #polygonize!!!
        p<-gdal_polygonizeR(wat)
        #p<<-p

        #         assign(filename,p)
        #         assign(filename,p,envir = globalenv())
        #         #Calculated Area
        Area.est<-sum(raster::area(x=p))/1e6

        #save.l<-list(name=filename,save.as=paste(filename,".RData",sep=""))

        #Error
        #save(list=save.l$name[1],file=save.l$save.as[1],ascii=FALSE)
        print(paste("Area Reported:",Area, "km2"))
        print(paste("Area Estimated:",round(Area.est,0),"km2"))
        print(paste("Percentage Error with respect to reported area: ",round((Area.est-Area)/Area*100,1),"%",sep=""))

        #Results from Watershed.att
        results<-watershed.att(p, Figure=FALSE)
        #Storage watershed
        results$Watershed<-p
        results$Watershed@proj4string<-CRS.bil

        results$Area_Est_km2<-round(Area.est,0)

        results$Area_Off_km2<-Area

        results$Area_Diff_pct<-round((Area.est-Area)/Area*100,1)

        #Watercourses in Figure
        snet.shp<-readShapeSpatial("logannet2.shp")
        snet.shp@proj4string<-CRS.bil
        snet.shp<-spTransform(snet.shp,CRS.longlat)
        snet.db<-fortify(snet.shp,id="watercourses")

        #Discharge point
        outlet<-cbind(Easting=approxpt$shp[2],Northing=approxpt$shp[3])
        outlet<-SpatialPoints(outlet,proj4string = CRS.bil)
        outlet<-spTransform(outlet,CRS.longlat)
        outlet.df<-as.data.frame(outlet@coords)

        #Temp<-results$Fig
        #results$Fig<-NULL

        #results$Fig<-Temp+
        #        geom_path(data=snet.db,aes(x=long,y=lat,group=group),color="blue",size=1.5)
        #geom_point(data=outlet.df,aes(x=Longitude,y=Latitude),color="red",size=4)

        if (exists("NAflow.wat")==FALSE){
                NAflow.wat<-NULL
        }else{
                file.remove(paste("NAflow.wat-",length(NAflow.wat),"Elem.RData",sep=""))

        }

        NAflow.wat[[make.names(filename)]]<-results

        NAflow.wat<<-NAflow.wat

        save(NAflow.wat,file=paste("NAflow.wat-",length(NAflow.wat),"Elem.RData",sep=""),ascii=FALSE,compress = TRUE)

        results

}

#' Watershed parameters
#'
#' @export
watershed.att<-function(polygon, Figure=TRUE){
        library(ggplot2)
        library(raster)
        library(geosphere)
        library(rgeos)
        library(scales)
        library(ggmap)

        file.topo<-"E:/Climate Change/GMTED2010/NorthAmerica_GMTED_med075-fill.bil"
        topo<-raster(file.topo)

        polygon<-gUnaryUnion(polygon)

        proj4string(polygon)<-topo@crs
        polygon.latlon<-spTransform(polygon,CRS("+init=epsg:4326"))

        #crop the complete topo raster
        cropped<-crop(topo,polygon)

        #mask the cropped raster
        base<-mask(cropped,polygon)
        base.latlon<-projectRaster(base,crs=CRS("+init=epsg:4326"))

        #Average elevation
        avg.elev<-cellStats(base,stat = mean)

        #Slope
        raster.slope<-terrain(base,opt=c("slope"),unit="degrees",neighboars=8)
        avg.slope<-cellStats(raster.slope,stat=mean)


        #Aspect calculation
        raster.aspect<-terrain(base,opt=c("aspect"),unit="radians",neighboars=8)

        raster.cos<-calc(raster.aspect,fun =cos)
        sum.cos<-cellStats(raster.cos,stat=sum)
        raster.sin<-calc(raster.aspect,fun =sin)
        sum.sin<-cellStats(raster.sin,stat=sum)

        arctan<-atan2(sum.sin,sum.cos)

        aspect<-arctan/pi*180
        aspect<-(aspect + 360) %% 360

        #Area
        area.val<-sum(raster::area(polygon))/1e6

        #plotting
        slope.fig<- terrain(base.latlon, opt='slope')
        aspect.fig<- terrain(base.latlon, opt='aspect')

        hill<- hillShade(slope.fig, aspect.fig, 40, 270)

        #   Convert rasters TO dataframes for plotting with ggplot
        hdf <- rasterToPoints(hill)
        hdf <- data.frame(hdf)
        colnames(hdf) <- c("X","Y","Hill")
        ddf <- rasterToPoints(base.latlon)
        ddf <- data.frame(ddf)
        colnames(ddf) <- c("X","Y","DEM")

        b.hs <- seq(min(hdf$Hill),max(hdf$Hill),length.out=100)
        b.dem <- seq(min(ddf$DEM),max(ddf$DEM),length.out=100)


        #         Fig1<-ggplot(NULL, aes(X, Y)) +
        #                 geom_raster(data = ddf, aes(fill = DEM)) +
        #                 geom_raster(data = hdf, aes(alpha = Hill)) +
        #                 scale_fill_gradientn(name="Altitude",colours = rainbow(20))+
        #                 guides(fill = guide_colorbar()) +
        #                 scale_alpha(range = c(0, 0.5), guide = "none") +
        #                 scale_x_continuous(labels=comma,name=expression(paste("Latitude (",degree,")")), expand=c(0,0)) +
        #                 scale_y_continuous(labels=comma,name=expression(paste("Longitude (",degree,")")), expand=c(0,0)) +
        #                 coord_equal()
        #
        #         #Fig1
        if(area.val>=2500){zoom.google="auto"}
        if(area.val<2500&area.val>1000){zoom.google<-9}
        if(area.val<1000){zoom.google<-10}


        map<-get_map(bbox(polygon.latlon),scale=2,zoom=zoom.google,maptype="hybrid",source="google",messaging=FALSE)

        #ggmap(map)

        Fig2<-ggmap(map)+
                geom_raster(data = ddf, aes(X,Y,fill = DEM,alpha=0.6)) +
                geom_raster(data = hdf, aes(X,Y,alpha = Hill)) +
                scale_fill_gradientn(name="Altitude",colours = rainbow(20))+
                guides(fill = guide_colorbar()) +
                scale_alpha(range = c(0, 0.5), guide = "none") +
                scale_x_continuous(labels=comma,name=expression(paste("Latitude (",degree,")")) ) +
                scale_y_continuous(labels=comma,name=expression(paste("Longitude (",degree,")"))) +
                coord_equal()

        suppressWarnings(print(Fig2))

        #perimeter
        perimeter.val<-perimeter(polygon.latlon)/1000

        #compactness coeff
        Kc<-0.28*perimeter.val/(sqrt(area.val))

        centroid.val<-gCentroid(polygon.latlon)@coords
        colnames(centroid.val)<-c("Longitude","Latitude")

        if (Figure==TRUE){
                result<-list(Avg.Elev_m=avg.elev,Avg.Slope_deg=avg.slope,Aspect_deg=aspect,Area_Est_km2=area.val,Perimeter_km=perimeter.val,Compacteness.Coef.=Kc,Centroid=centroid.val,Fig=Fig2)
        } else{
                result<-list(Avg.Elev_m=avg.elev,Avg.Slope_deg=avg.slope,Aspect_deg=aspect,Area_Est_km2=area.val,Perimeter_km=perimeter.val,Compacteness.Coef.=Kc,Centroid=centroid.val)
        }


        result


}



#' Transform from a raste to a vector with gdal
#'
#' @export

gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
        if (isTRUE(readpoly)) require(rgdal)
        if (is.null(pypath)) {
                pypath <- Sys.which('gdal_polygonize.py')
                #pypath<-"C:/Program Files/GDAL"
        }
        if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
        owd <- getwd()
        on.exit(setwd(owd))
        setwd(dirname(pypath))
        if (!is.null(outshape)) {
                outshape <- sub('\\.shp$', '', outshape)
                f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
                if (any(f.exists))
                        stop(sprintf('File already exists: %s',
                                     toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                                    sep='.')[f.exists])), call.=FALSE)
        } else outshape <- tempfile()
        if (is(x, 'Raster')) {
                require(raster)
                writeRaster(x, {f <- tempfile(fileext='.asc')})
                rastpath <- normalizePath(f)
        } else if (is.character(x)) {
                rastpath <- normalizePath(x)
        } else stop('x must be a file path (character string), or a Raster object.')
        system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                        pypath, rastpath, gdalformat, outshape)))
        if (isTRUE(readpoly)) {
                shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
                return(shp)
        }
        return(NULL)
}

