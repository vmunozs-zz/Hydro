#Matrixplot
#matrixplot(flow.dwi, ColorRamp = "Days")

#hydropairs
#hydropairs(as.data.frame(flow.daily.comp),dec=3,use="pairwise.complete.obs")

#hydropairs
#hydropairs(as.data.frame(ECm.st.totp.comp),dec=3,use="pairwise.complete.obs",)


#function to obtain r2 in graph
#'@export
lm_eqn <- function(df){
        m = lm(MAP ~ Elevation, df);
        eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                         list(a = format(coef(m)[1], digits = 4),
                              b = format(coef(m)[2], digits = 4),
                              r2 = format(summary(m)$r.squared, digits = 3)))
        as.character(as.expression(eq));
}


#MAP versus Elevation


#'@export
MAPplot<-function(){
        library("ggplot2")
        library("gridExtra")
        library("sp")
        ECm.annual.comp<-NOAA.stations.syn$annual.avg.table
        ECm.monthly.data<-NOAA.stations.syn$monthly.avg
        ECm.monthly.data.nd<-NOAA.stations.syn$monthly.avg.nd
        selected.m.network<-selected.NOAA.network

        graph.space.p1<-bbox(cbind(ECm.annual.comp$Coast.Distance,ECm.annual.comp$MAP))

        legend.location<<-c(((graph.space.p1[1,2]-graph.space.p1[1,1])*0.3+graph.space.p1[1,1]),
                            ((graph.space.p1[2,2]-graph.space.p1[2,1])*0.7+graph.space.p1[2,1]))

        site.pair<-list(c(point.location[2],point.location[1]))
        site.elevation<<-GE.elev(longitude = point.location[1],latitude=point.location[2])
        site.coast.distance<<-coast.dist(longitude = point.location[1],latitude=point.location[2])
        graph.line <- lm(ECm.annual.comp$MAP~ECm.annual.comp$Coast.Distance)
        site.map<<-site.coast.distance*graph.line$coefficients[[2]]+graph.line$coefficients[[1]]


        p1 <- ggplot(data = ECm.annual.comp, aes(x = Coast.Distance, y = MAP, color=Distance, label=Name))+
                geom_point(size=4)+#scale_fill_continuous(low="red",high="red")+
                #Best Fit line
                geom_smooth(method = "lm", se=TRUE, color="black", formula = y ~ x) +
                #range for x
                scale_x_continuous(limits=c(graph.space.p1[1,1]*0.9,graph.space.p1[1,2]*1.1))+
                #range for y
                #scale_y_continuous(limits=c(0,graph.space.p1[2,2]*1.1))+
                #text for stations
                geom_text(aes(label=Name),hjust=-0.1, vjust=-0.1, size=3, alpha=0.8)+
                #legend
                geom_text(aes(x = legend.location[1], y = legend.location[2], label = lm_eqn(ECm.annual.comp)), parse = TRUE)+
                #Titles
                xlab("Coast Distance[km]")+ylab("Mean Annual Precipitation[mm/yr]")+
                #site
                geom_point(aes(x=site.coast.distance,y=site.map, label="Site"),color="red",size=6)
        #geom_text(aes(label="Site"),color="red",hjust=-0.1, vjust=-0.1, size=4, alpha=1)

        #Monthly Distribution

        #Info Preparation
        monthly.data<-data.frame(matrix(ncol=1,nrow=12*nrow(ECm.monthly.data),data = unlist(t(ECm.monthly.data.nd[2:13]))))
        stations<-rep(selected.m.network$Station,each=12)
        months<-rep(1:12,times = (nrow(ECm.monthly.data)))

        monthly.graphs<-cbind(stations,months,monthly.data)
        names(monthly.graphs)<-c("Station","Months","Pp.Distribution")

        #Site monthly Average
        site.monthly.map<<-round(site.map*colMeans(ECm.monthly.data.nd[2:13]),1)
        site.monthly.map.df<-data.frame(1:12,colMeans(ECm.monthly.data.nd[2:13]))
        names(site.monthly.map.df)<-c("Months","Pp.Distribution")


        p2<-ggplot(data=monthly.graphs, aes(x=Months,y=Pp.Distribution,shape=Station,colour=Station))+
                geom_line()+ scale_colour_hue()+geom_point(size=5)+
                scale_shape_manual(values=LETTERS[1:26])+
                scale_x_discrete()+guides(col=guide_legend(nrow=18))

           # p2<-  p2+  geom_line(data=site.monthly.map.df, aes(x=Months,y=Pp.Distribution))



        #Map
        library("RgoogleMaps")
        library("ggmap")
        library("OpenStreetMap")

        #Coordinates definition for Google Map
        coords<-cbind(ECm.annual.comp$Longitude,ECm.annual.comp$Latitude)
        coords<-rbind(coords,point.location)
        coords<-bbox(coords)

        #define max and min ranges (extend from bbox)
        xdelta<-0.15*(coords[1,2]-coords[1,1])
        ydelta<-0.15*(coords[2,2]-coords[2,1])

        coords[1,1]<-coords[1,1]-xdelta
        coords[1,2]<-coords[1,2]+xdelta

        coords[2,1]<-coords[2,1]-ydelta
        coords[2,2]<-coords[2,2]+ydelta


        google.map<-openmap(c(coords[2,2],coords[1,1]),c(coords[2,1],coords[1,2]),minNumTiles=12,type="mapquest")

        #Project in lat and long
        google.map<-autoplot(openproj((google.map)))

        #Arrangements for place the site
        location<-as.data.frame(cbind(t(point.location),"SITE"),stringsAsFactors = FALSE)
        location[1:2]<-as.numeric(location[1:2])
        names(location)<-c("Longitude","Latitude", "Label")


        p3<-google.map+geom_point(data=ECm.annual.comp, aes(x=Longitude,y=Latitude,label=Name, color=MAP, size=3), alpha=0.5)+
                scale_colour_gradientn(colours=rainbow(3))+
                #Label definition
                geom_text(data=ECm.annual.comp, aes(x=Longitude,y=Latitude,label=Name, angle=20,size=3,vjust=-0.1,hjust=-0.1))+
                scale_size_continuous(guide=FALSE)+

                #Site definition
                geom_point(data=location, aes(x=Longitude,y=Latitude,label=Label),color="red", alpha=1)+coord_equal()+
                geom_text(data=location, aes(x=Longitude,y=Latitude,label=Label, face="bold", vjust=-0.1,hjust=-0.1))+
                xlab("Longitude[dec.degrees]")+ylab("Latitude[dec.degrees]")


        grid.arrange(p3, nrow=1, arrangeGrob(p1,p2,nrow=2))
}

#'@export
Normalplot<-function(){
        #Map
        library("RgoogleMaps")
        library("ggmap")
        library("OpenStreetMap")

        #Coordinates definition for Google Map
        coords<-cbind(selected.n.network$Longitude,selected.n.network$Latitude)
        coords<-rbind(coords,point.location)
        coords<-bbox(coords)

        #define max and min ranges (extend from bbox)
        xdelta<-0.15*(coords[1,2]-coords[1,1])
        ydelta<-0.15*(coords[2,2]-coords[2,1])

        coords[1,1]<-coords[1,1]-xdelta
        coords[1,2]<-coords[1,2]+xdelta

        coords[2,1]<-coords[2,1]-ydelta
        coords[2,2]<-coords[2,2]+ydelta


        google.map<-openmap(c(coords[2,2],coords[1,1]),c(coords[2,1],coords[1,2]),minNumTiles=15,type="mapquest")

        #Project in lat and long
        google.map<-autoplot(openproj((google.map)))

        #Arrangements for place the site
        location<-as.data.frame(cbind(t(point.location),"SITE"),stringsAsFactors = FALSE)
        location[1:2]<-as.numeric(location[1:2])
        names(location)<-c("Longitude","Latitude", "Label")


        p3<-google.map+geom_point(data=selected.n.network, aes(x=Longitude,y=Latitude,label=Station, color=Parameters, size=3), alpha=0.5)+
                scale_colour_gradientn(colours=rainbow(3))+
                #Label definition
                geom_text(data=selected.n.network, aes(x=Longitude,y=Latitude,label=Station, angle=20,size=3,vjust=-0.1,hjust=-0.1))+
                scale_size_continuous(guide=FALSE)+

                #Site definition
                geom_point(data=location, aes(x=Longitude,y=Latitude,label=Label),color="red", alpha=1)+coord_equal()+
                geom_text(data=location, aes(x=Longitude,y=Latitude,label=Label, face="bold", vjust=-0.1,hjust=-0.1))+
                xlab("Longitude[dec.degrees]")+ylab("Latitude[dec.degrees]")

        p3

}

#'@export
hydroplot.ts<-function(i=1){
        ts<-zoo(ECm.st.totp[[i]][,2],ECm.st.totp[[i]][,1])
        hydroplot(ts,var.unit = "mm",
                  FUN=sum,
                  main=selected.m.network$Station[i],
                  ptype="ts+boxplot")
}

