#'Flow Patch function. Allows to patch between two different stations
#'@param    df Database name
#'@param    areas Watershed Areas
#'@param    to.be.patched Station to be patched, id location inside the database
#'@param    with Station to be patched with, id location inside the database
#'@return   poly.n Polynomial regression. n<1 Lowess regression, n=>1 Polynomial regression with n degrees.
#'@export

Flow.patch<-function(df=flow.all.zoo, areas,to.be.patched=2,with=4, poly.n=0.75){
            library(ggplot2)
            library(scales)
            library(copula)
            library(proto)
            library(grid)

            print(paste(names(df)[with],"->",names(df)[to.be.patched]))

            time_ts<-time(df)

            y<-df[,to.be.patched]
            x<-df[,with]

            x_y<-data.frame(x,y)

            x_y<-x_y[complete.cases(x_y),]

            #initial variable
            y2<-y

            #Polynomial regression
            if(poly.n>=1){

                        model<-lm(x_y$y~stats::poly(x_y$x,poly.n,raw=TRUE))

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
            if(poly.n>=1){

                        reg.df<-data.frame(x=x.predict,
                                           y=polynEval(model$coefficients,x.predict))
            }else{
                        reg.df<-data.frame(x=x.predict,
                                           y=predict(model,x.predict))

            }




            #Monthly graph
            monthly.y2<-daily2monthly.V2(x = y2,FUN = mean, na.rm=TRUE,max.miss=5)/areas[to.be.patched]
            monthly.y<-daily2monthly.V2(x = y,FUN = mean, na.rm=TRUE,max.miss=5)/areas[to.be.patched]
            monthly.x<-daily2monthly.V2(x = x,FUN = mean, na.rm=TRUE,max.miss=5)/areas[with]

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
                        labs(y=paste(names(df)[to.be.patched],"[m3/s]"),
                             x=paste(names(df)[with],"[m3/s]"),
                             title="Daily - Patching Relationship - Linear Scale")+
                        theme(plot.title=element_text(size=16,vjust=2))+
                        geom_path(data=reg.df,aes(x=x,y=y),color="red",size=1)+
                        geom_text(data=r2.df,mapping=aes(x=x,y=y,label=text))

            Fig2<-ggplot(x_y)+
                        geom_point(aes(x=x,y=y),size=0.1)+
                        labs(y=paste(names(df)[to.be.patched],"[m3/s]"),
                             x=paste(names(df)[with],"[m3/s]"),
                             title="Daily - Patching Relationship - Logarithmic Scale")+
                        theme(plot.title=element_text(size=16,vjust=2))+
                        geom_path(data=reg.df,aes(x=x,y=y),color="red",size=1)+
                        geom_text(data=r2.df,mapping=aes(x=x,y=y,label=text))+
                        scale_y_log10()+
                        scale_x_log10()+
                        annotation_logticks()


            Fig3<-ggplot(monthly.xy,aes(x=monthly.x,y=monthly.y))+
                        geom_point()+
                        labs(y=paste(names(df)[to.be.patched],"[m3/s/km2]"),
                             x=paste(names(df)[with],"[m3/s/km2]"),
                               title="Monthly Avg. - Original Evaluation")+
                        theme(plot.title=element_text(size=16,vjust=2))+
                        #stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)+
                        geom_smooth(method=lm)



            Fig4<-ggplot(monthly.xy2,aes(x=monthly.x,y=monthly.y2))+
                        geom_point()+
                        labs(y=paste(names(df)[to.be.patched],"- PATCHED [m3/s/km2]"),
                             x=paste(names(df)[with],"[m3/s/km2]"),
                             title="Monthly Avg. - Patched Evaluation")+
                        theme(plot.title=element_text(size=16,vjust=2))+
                        #stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE)+
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
            pushViewport(viewport(layout = grid.layout(3, 4)))
            print(Fig1,vp=vplayout(3,1))
            print(Fig2,vp=vplayout(3,2))
            print(Fig3,vp=vplayout(3,3))
            print(Fig4,vp=vplayout(3,4))

            print(Fig5,vp=vplayout(1,1:3))
            print(Fig6,vp=vplayout(2,1:3))
            print(Fig7,vp=vplayout(1:2,4))
            results<-list(y,y2[start:end],r2,names(df)[with],names(df)[to.be.patched],Fig1,Fig2,Fig3,Fig4,Fig5,Fig6,Fig7)
            names(results)<-c("original.ts","patched.ts","r2","with","patched","Fig1","Fig2","Fig3","Fig4","Fig5","Fig6","Fig7")

            results
}
