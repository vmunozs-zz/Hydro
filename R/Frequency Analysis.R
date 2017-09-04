# Frequency Analysis ------------------------------------------------------
#'Frequency Analysis from Clipboard  information by columns
#'
#'Estimate the Frequency Analysis from Clipboard information by columns
#'@param          information from clipboard
#'@return       Frequency analysis results at the clipboard
#'@export
FA.clip<-function(){
            #print("No header were used")
            b<-read.excel(header = F,stringsAsFactors = F)

            b<-as.matrix(b)
            a<-split(b,col(b))

            #Combined
            results.combined<-mapply(FA,a)

            result.list<-results.combined[4,]

            #Capture de probabiities colum
            result.prob<-result.list[[1]]$Prob

            #Remove extra colum
            FA.results<-lapply(result.list,FUN=function(x){x[,2]})

            result.names<-lapply(result.list,FUN=function(x){names(x)[2]})

            FA.result.df<-data.frame(FA.results)
            names(FA.result.df)<-paste(names(FA.result.df),unlist(result.names),sep=".")

            result<-data.frame(result.prob,FA.result.df)
            result

            write.table(result, "clipboard", sep="\t", row.names=FALSE, col.names=TRUE)
}


#'Frequency Analysis
#'
#'Estimate the Frequency Analysis
#'@param a         numerical vector
#'@param display   Option for graphical output (T/F)
#'@export
FA<-function(a,distrib=c("NORM","LN","GEV","GUMBEL","P3","LP3"),display=FALSE){
            library("nsRFA")

            #remove NA
            a<-a[!is.na(a)]

            #Values
            #a<-c( 43.9,53.8,58.9,64,64.1,65.8,70.8,78.1,78.2,85.8,88.6,89.5,93.6,93.9,95.1,95.4,
            #96.1,98.4,99.1,101,103,111,111,116,128,128,129,132,133,156,163,182,184)

            ll<-Lmoments(a)
            lle<-Lmoments(log(a))
            dist<-NULL
            #Normal
            dist[1]<-list(ML_estimation(a,dist="NORM"))
            #LogNormal
            dist[2]<-list(par.lognorm(ll[1],ll[2],ll[4]))
            #GEV
            dist[3]<-list(par.GEV(ll[1],ll[2],ll[4]))
            #Gumbel
            dist[4]<-list(par.gumb(ll[1],ll[2]))
            #Pearson III
            dist[5]<-list(par.gamma(ll[1],ll[2],ll[4]))
            #Log Pearson III
            dist[6]<-list(par.gamma(lle[1],lle[2],lle[4]))

            names(dist)<-c("NORM","LN","GEV","GUMBEL","P3","LP3")

            used.dist<-distrib

            #Probabilities
            x<-c(0.9999,0.9995,0.9990,0.9980,0.9950,0.9900,0.9800,0.9600,0.9500,0.9000,0.8000,0.5708,0.5000,
                 0.3333,0.2000,0.1000,0.0500,0.0400,0.0200,0.0100,0.0050,0.0020,0.0010,0.0005,0.0001)
            #Normal
            result<-NULL
            result[1]<-list(qnorm(x,mean = dist$NORM[1],sd = dist$NORM[2]))
            #Log Normal
            result[2]<-list(invF.lognorm(x,xi = dist$LN$xi,alfa = dist$LN$alfa,k = dist$LN$k))
            #GEV
            result[3]<-list(invF.GEV(x,xi = dist$GEV$xi,alfa = dist$GEV$alfa,k = dist$GEV$k))
            #Gumbel
            result[4]<-list(invF.gumb(x,xi = dist$GUMBEL$xi,alfa = dist$GUMBEL$alfa))
            #Pearson III
            #print(dist$P3$beta)
            if(!is.na(dist$P3$beta)){
                        result[5]<-list(invF.gamma(x,xi = dist$P3$xi,beta = dist$P3$beta,alfa = dist$P3$alfa))

            } else{
                        result[5]<-list(rep(x = NA,times = length(x)))
                        used.dist<-used.dist[used.dist!="P3"]
            }
            #Log Pearson III
            if(!is.na(dist$LP3$beta)){
                        result[6]<-list(exp(invF.gamma(x,xi = dist$LP3$xi,beta = dist$LP3$beta,alfa = dist$LP3$alfa)))
            } else{
                        result[6]<-list(rep(x = NA,times = length(x)))
                        used.dist<-used.dist[used.dist!="LP3"]
            }

            if(sum(is.na(ML_estimation(log(a),dist="GAM")))==2){
                        used.dist<-used.dist[used.dist!="P3"]
                        used.dist<-used.dist[used.dist!="LP3"]
            }

            report<-data.frame(x,result[1:6])
            names(report)<-c("Prob","NORM","LN","GEV","GUMBEL","P3","LP3")

            #Fitness
            #print(used.dist)

            fitness<-MSClaio2008(a,dist=used.dist)# ,crit=c("AICc", "BIC", "ADC"))

            #Distribution selected
            selected.dist<-rmode(c(fitness$AICcdist,fitness$BICdist,fitness$ADCdist,fitness$AICdist))

            #if there is not agreement in the selected distribution is selected the Bayesian Information Criterion
            if(length(selected.dist)!=1){
                        selected.dist<-fitness$BICdist
            }


            selected<-data.frame(report$Prob,report[eval(selected.dist)])
            names(selected)[1]<-c("Prob")

            results<-list(a, report, fitness,selected)
            names(results)<-c("sample","report","fitness","results")

            if(display==TRUE){plot(fitness)}

            summary(fitness)

            results
}

#' Statistical mode
#'
#' Statistical mode - Used to estimate the best regression
#' @param x Vector to estimate the statistical mode
#' @return Return the statistical mode
rmode <- function(x) {
            x <- sort(x)
            u <- unique(x)
            y <- lapply(u, function(y) length(x[x==y]))
            u[which( unlist(y) == max(unlist(y)) )]
}



#'Frequency analysis for flows at one station
#'
#'Estimate frequency analysis for peak flows at one station
#'@param stationid Station id to prepare the frequency analysis required a flow analysis results
#'@export
FA.ECflow.1<-function(stationid){
            library("ggplot2")
            if(exists("NAflow.db")==TRUE){

                        ECflow.db<-NAflow.db
                        ECflow.idx<-NAflow.idx
                        ECflod.syn<-NAflow.syn

            }

            i<-match(paste("X",stationid,sep=""),names(ECflow.db$annual.maxima))
            j<-match(paste("X",stationid,sep=""),names(ECflow.db$annual.peak))

            # i<-17


            #check for consistency
            if(!identical(i,j)){stop("Invalid variables")}

            if(length(ECflow.db$annual.peak[[i]])==0|is.null(ECflow.db$annual.maxima[[i]])){

                        values<-c(0.9999,0.9995,0.9990,0.9980,0.9950,0.9900,0.9800,0.9600,0.9500,0.9000,0.8000,0.5708,0.5000,
                                  0.3333,0.2000,0.1000,0.0500,0.0400,0.0200,0.0100,0.0050,0.0020,0.0010,0.0005,0.0001)
                        na.values<-rep(x = NA,times = length(values))
                        y<-data.frame(values,na.values)
                        names(y)<-c("Prob",names(ECflow.db$annual.peak[i]))

                        report<-list(y)
                        names(report)<-"peak.flows"
                        report

            } else{


                        #defining tables
                        year.am<-unique(year(as.Date(time(ECflow.db$annual.maxima[[i]]))))
                        year.ap<-unique(year(as.Date(time(ECflow.db$annual.peak[[i]]))))


                        am<-data.frame(year=year.am,
                                       annual.maxima=as.numeric(coredata(ECflow.db$annual.maxima[[i]][,1])),
                                       stringsAsFactors = FALSE)


                        ap<-data.frame(year=year.ap,
                                       annual.peak=as.numeric(coredata(ECflow.db$annual.peak[[i]][,1])),
                                       stringsAsFactors = FALSE)


                        peak<-merge(am,ap,all = TRUE)

                        peak<-data.frame(peak, peak$annual.peak)
                        names(peak)<-c("year","annual.maxima","annual.peak","estimated.peak")

                        fit<-lm(peak$annual.peak ~ peak$annual.maxima )

                        summary(fit)

                        interpo<-is.na(peak$estimated.peak)

                        estimated<-round(peak$annual.maxima*fit$coefficients[2]+fit$coefficients[1],1)

                        # a minimum value of r2=0.8 is used to patch
                        if(summary(fit)$r.squared>=0.8){
                                    peak$estimated.peak[interpo]<-pmax(estimated[interpo],peak$annual.maxima[interpo])
                        }else{
                                    peak$estimated.peak[interpo]<-NA
                        }


                        #graph just with complete cases
                        peak.graph<-peak[complete.cases(peak),]

                        #function for r?


                        #graph
                        a<-ggplot(data =peak.graph, aes(x=annual.maxima, y=annual.peak))+
                                    geom_point(size=4)+

                                    geom_smooth(method = "lm", se=TRUE, color="black", formula = y ~ x)+
                                    #labels
                                    xlab("Annual Maxima [m3/s]")+ylab("Annual Peak [m3/s]")


                        #Variables
                        variables<-data.frame(fit$coefficients[2],fit$coefficients[1],summary(fit)$r.squared)
                        names(variables)<-c("m","n","r2")
                        row.names(variables)<-"var"

                        report<-list(peak,variables,a)
                        names(report)<-c("peak.flows","variables","graph")

                        report
            }
}

#'Frequency analysis for flows at the complete work database
#'
#'Flow Analysis provide results of a complete database of peak flows
#'@export

FA.ECflow.all<-function(){
            library("lubridate")

            #Patching for Northeamerica DB
            if(exists("NAflow.db")==TRUE){

                        ECflow.db<-NAflow.db
                        ECflow.idx<-NAflow.idx
                        ECflod.syn<-NAflow.syn
                        print("North-American DB")

            }else{
                        print("Canadian DB")
            }

            peak<-lapply(as.list(ECflow.idx$id),FUN=function(x){

                        Peak.flow<-FA.ECflow.1(x)

                        if(is.null(Peak.flow$peak.flows$estimated.peak)){
                                    flow.freqa<-NULL
                        } else{
                                    if(sum(!is.na(Peak.flow$peak.flows$estimated.peak))>=10){
                                                flow.freqa<-FA(Peak.flow$peak.flows$estimated.peak)
                                    }else{
                                                flow.freqa<-NULL
                                    }
                        }

                        result<-list(Peak.flow,flow.freqa)
                        names(result)<-c("input","analysis")
                        result

            })
            names(peak)<-ECflow.idx$id

            values<-c(0.9999,0.9995,0.9990,0.9980,0.9950,0.9900,0.9800,0.9600,0.9500,0.9000,0.8000,0.5708,0.5000,
                      0.3333,0.2000,0.1000,0.0500,0.0400,0.0200,0.0100,0.0050,0.0020,0.0010,0.0005,0.0001)

            rp.text<-c("Wet_10,000y","Wet_2,000y","Wet_1,000y","Wet_500y","Wet_200y","Wet_100y", "Wet_50y",
                       "Wet_25y", "Wet_20y", "Wet_10y","Wet_5y","Avg.","Wet_2y", "Dry_3y", "Dry_5y",
                       "Dry_10y","Dry_20y","Dry_25y", "Dry_50y", "Dry_100y", "Dry_200y", "Dry_500y",
                       "Dry_1,000y","Dry_2,000y","Dry_10,000y")

            flow.freq<-lapply(peak,FUN=function(x){
                        if(is.list(x$analysis)){

                                    x$analysis$results[,2]
                        } else{
                                    rep(x = NA,times = length(values))

                        }

            })

            flow.freq.table<-cbind(rp.text,values,as.data.frame(do.call(cbind,flow.freq)))
            names(flow.freq.table)<-c("Return Period","Prob",ECflow.idx$id)


            #Non dimensional freq analysis
            flow.freq.table.nd<-flow.freq.table
            flow.freq.table.nd[,3:(nrow(ECflow.idx)+2)]<-flow.freq.table[,3:(nrow(ECflow.idx)+2)]/
                        matrix(rep(ECflow.idx$areagross,times = 25),ncol = nrow(ECflow.idx),byrow = T)


            #output figure

            #Select the available stations
            flow.freq.graph<-t(flow.freq.table.nd)

            #preparation of data
            if(exists("ECflow.syn")==TRUE){
                        graph<-data.frame(ECflow.idx,
                                          MAR<-ECflow.syn$MAR.table,
                                          elevation<-ECf.GE.elev(),
                                          coast_dist<-ECf.coast.dist(),
                                          as.data.frame(flow.freq.graph[c(-1,-2),],stringsAsFactors = FALSE),stringsAsFactors = FALSE)

                        #removed repeated column
                        names(graph)[16]<-"coast_dist"
                        names(graph)[17:41]<-c("Wet_10,000y","Wet_2,000y","Wet_1,000y","Wet_500y","Wet_200y","Wet_100y", "Wet_50y",
                                               "Wet_25y", "Wet_20y", "Wet_10y","Wet_5y","Avg.","Wet_2y", "Dry_3y", "Dry_5y",
                                               "Dry_10y","Dry_20y","Dry_25y", "Dry_50y", "Dry_100y", "Dry_200y", "Dry_500y",
                                               "Dry_1,000y","Dry_2,000y","Dry_10,000y")



                        #transform to numeric
                        graph[,17:41]<-sapply(graph[,17:41],as.numeric)
                        #         flow.freq.graph<<-graph


                        ECflow.FA<-list(peak,flow.freq.table,flow.freq.table.nd,graph)
                        names(ECflow.FA)<-c("peakflow.patching.review","FA.table","FA.nd.table","FA.review.table")
                        ECflow.FA<<-ECflow.FA

                        #Output figure
                        graph2<-ECflow.FA$FA.review.table
                        graph2<-graph2[,c(4:6,14,15,16,29:36)]
                        hydropairs(graph2,method = "spearman",dec = 1)
            }

            #preparation of data
            if(exists("NAflow.syn")==TRUE){
                        graph<-data.frame(ECflow.idx,
                                          MAR<-NAflow.syn$MAR.table,
                                          elevation<-ECf.GE.elev(),
                                          coast_dist<-ECf.coast.dist(),
                                          as.data.frame(flow.freq.graph[c(-1,-2),],stringsAsFactors = FALSE),stringsAsFactors = FALSE)

                        #removed repeated column
                        #graph<-graph[-14]
                        names(graph)[17]<-"coast_dist"
                        names(graph)[18:42]<-c("Wet_10,000y","Wet_2,000y","Wet_1,000y","Wet_500y","Wet_200y","Wet_100y", "Wet_50y",
                                               "Wet_25y", "Wet_20y", "Wet_10y","Wet_5y","Avg.","Wet_2y", "Dry_3y", "Dry_5y",
                                               "Dry_10y","Dry_20y","Dry_25y", "Dry_50y", "Dry_100y", "Dry_200y", "Dry_500y",
                                               "Dry_1,000y","Dry_2,000y","Dry_10,000y")



                        #transform to numeric
                        graph[,18:42]<-sapply(graph[,18:42],as.numeric)
                        #         flow.freq.graph<<-graph


                        NAflow.FA<-list(peak,flow.freq.table,flow.freq.table.nd,graph)
                        names(NAflow.FA)<-c("peakflow.patching.review","FA.table","FA.nd.table","FA.review.table")
                        NAflow.FA<<-NAflow.FA

                        #Output figure
                        graph2<-NAflow.FA$FA.review.table
                        graph2<-graph2[,c(4:6,14,15,16,17,30:22)]
                        hydropairs(graph2,method = "spearman",dec = 1)
            }


}

#'Frequency analysis graph for results at flow.freq.graph
#'
#'Frequency analysis graph for results at flow.freq.graph
#'@param event        string of the event based on the titles from table flow.freq.graph
#'@param max.distance number in km for the maximum distance considered for the graph
#'@export
FA.ECflow.graph<-function(event="Dry_200y", max.distance=400){
            library("ggplot2")
            #event<<-event
            # Letter size
            flow.freq.graph<-ECflow.FA$FA.review.table
            #jpeg("test4.jpg", width=8.5, height=11, units="in", res=500)
            flow.freq.graph<-flow.freq.graph[flow.freq.graph$distance.km<max.distance,]
            #signif(max(flow.freq.graph[event]),1)
            graph.title<-paste("Unit Peak Flow for ",gsub("_"," ",event),
                               " at Latitude ",round(point.location[2],2),", Longitude ",round(point.location[1],2),sep="")

            p1 <- ggplot(data = flow.freq.graph, aes_string(x = "areagross", y = event, color="distance.km", label="id"))+
                        scale_colour_gradient(name="Distance from \n Site[km]", limits=c(0,300))+
                        geom_point(size=1, shape=16)+
                        geom_smooth(method="loess", size=0.5, colour="red",alpha=0.4, ,span=1,linetype="dashed",fullrange=T)+

                        geom_smooth(method="lm", size=0.5, colour="blue",alpha=0.4,linetype="longdash",fullrange=T)+

                        geom_text(aes(label=id),hjust=-0.1, vjust=-0.1, size=4, alpha=1, angle=0)+

                        #stat_function(fun=function(x){pmin(0.48,4.6893*(10^x)^-0.34)},n=1000, size=1)+
                        scale_x_log10(breaks=c(1,2,3,4,5,10,20,50,100,200,500,1000,2000,5000,10000,50000,100000),
                                      limits=c(1,round(max(flow.freq.graph$areagross),-3)))+
                        #scale_y_continuous(breaks=seq(0,5,signif(max(flow.freq.graph[event]),1)),
                        #                           limits=c(0,max(flow.freq.graph[event])))+
                        annotation_logticks(sides="b")+
                        xlab("Watershed Area[km2]")+ylab("Unit Peak Flow[m3/s/km2]")+

                        ggtitle(graph.title)+
                        theme(plot.title = element_text(face="bold"))

            #assign(paste("graph.",event,sep=""),p1,envir = .GlobalEnv)

            p1

}


#'Frequency analysis for meteorology at the complete work database
#'
#'Frequency analysis results for the complete meteorologicaldatabase of peak flows
#'@param a             ECm.annual.peak.table
#'@param Dailyto24     Daily to 24 hours correction default of 1.13
#'@export
ECm.pf<-function(db=ECm.annual.peak.table, Dailyto24=1.13){
            #a<-ECm.annual.peak.table;Dailyto24<-1.13

            #Table Preparation
            peak.table<-db

            pp.freqa<-sapply(peak.table,FUN=function(x){

                        if(sum(!is.na(x))>=6){
                                    round(FA(x*Dailyto24)$results[,2],2)

                        }else{
                                    rep(NA,25)}

            })

            values<-c(0.9999,0.9995,0.9990,0.9980,0.9950,0.9900,0.9800,0.9600,0.9500,0.9000,0.8000,0.5708,0.5000,
                      0.3333,0.2000,0.1000,0.0500,0.0400,0.0200,0.0100,0.0050,0.0020,0.0010,0.0005,0.0001)
            rp.text<-c("Wet_10,000y","Wet_2,000y","Wet_1,000y","Wet_500y","Wet_200y","Wet_100y", "Wet_50y",
                       "Wet_25y", "Wet_20y", "Wet_10y","Wet_5y","Avg","Wet_2y","Dry_3y", "Dry_5y",
                       "Dry_10y","Dry_20y","Dry_25y", "Dry_50y", "Dry_100y", "Dry_200y", "Dry_500y",
                       "Dry_1,000y","Dry_2,000y","Dry_10,000y")

            return.period.df<-data.frame("Return Period"=rp.text,Probability=values)

            pp.freq.table<-cbind(return.period.df,as.data.frame(pp.freqa))
            freq.table<<-pp.freq.table

}

#'Estimate the PMP based on Herschfield Methodology
#'
#'PMP Estimation based on Hershfield methodology from Daily values
#'@param vector             Vector
#'@param input_hr      PMP to be defined 24 hrs, 6 hrs or 1 hr/
#'@param area_km     Watershed area. The methodology shows a graph for that information; however, it does not adjust the PMP for that correction
#'@param number_of_obs_units How many observation units are implemented. ie Daily info for 24 hrs PMP is 1.
#'@export

#Function prepared by VM


PMP<-function(vector,input_hrs=24,area_km=100,number_of_obs_unit=1){

            library(ggplot2)
            library(gridExtra)

            if(sum(input_hrs==c(24,6,1))==0){
                        print("input should be 24, 6 or 1 hr.")
            } else{

                        Max.year<-vector

                        #remove NA
                        X.n<-Max.year[!is.na(Max.year)]

                        X.max.location<-match(max(X.n),X.n)

                        X.n_m<-X.n[-X.max.location]

                        n<-length(X.n)
                        n<<-n
                        X<-list(c(X.n),c(X.n_m))

                        X.mean<-lapply(X,mean)
                        X.sd<-lapply(X,sd)

                        Xn_m_div<-X.mean[[2]]/X.mean[[1]]
                        Sn_m_div<-X.sd[[2]]/X.sd[[1]]
                        #Fig 4.2
                        Fig_4.2f<-function(n,Xn_m_div){((0.0018*n^2 - 0.2037*n + 106.53)*Xn_m_div+-0.00005945*n^3 + 0.00758*n^2 - 0.3111*n + 4.864)/100}
                        Fig_4.2<-Fig_4.2f(n = n,Xn_m_div = Xn_m_div)

                        #Figure 4.2 - GGPLOT
                        n_graph<-c(10,15,20,30,50)
                        Xn_m_graph<-seq(0.7,1,0.05)

                        Fig_4.2.df<-data.frame(Xn_m=rep(Xn_m_graph,5),
                                               n_graph=as.character(rep(n_graph,each=7)),
                                               Xn_adj=Fig_4.2f(n=rep(n_graph,each=7),Xn_m_div = rep(Xn_m_graph,5)))

                        Fig_4.2.site<-data.frame(Xn_m=Xn_m_div,n_graph=paste(n,"years"),Xn_adj=Fig_4.2)


                        Fg4.2<-ggplot(data=Fig_4.2.df,aes(x=Xn_m,y=Xn_adj))+
                                    geom_line(aes(color=n_graph,group=n_graph))+
                                    scale_x_continuous(breaks=seq(0.7,1,0.05))+
                                    scale_y_continuous(breaks=seq(0.7,1.1,0.05))+
                                    scale_color_discrete()+
                                    labs(x="Xn-m/Xn",y="Xn adjustment factor",color="Length of records\n[years]",
                                         title="Figure 4.2. Adjustment of mean annual series for\r
                                                maximum observed rainfall (Hershfield, 1961)")+
                                    geom_point(data=Fig_4.2.site,aes(x=Xn_m,y=Xn_adj),size=3,color="red")+
                                    geom_text(data=Fig_4.2.site,aes(x=Xn_m,y=Xn_adj,label=n_graph),vjust=-0.5)


                        #Fig 4.3
                        Fig_4.3f<-function(n,Sn_m_div){(Sn_m_div*(134.14*n^-0.053)+(+-0.0006781*n^2 + 0.01089*n + 0.3812))/100}

                        Fig_4.3<-Fig_4.3f(n,Sn_m_div)

                        #Figure 4.3 - GGPLOT
                        n_graph<-c(10,15,20,30,50)
                        Sn_m_graph<-seq(0.2,1,0.05)

                        Fig_4.3.df<-data.frame(Sn_m=rep(Sn_m_graph,5),
                                               n_graph=as.character(rep(n_graph,each=17)),
                                               Sn_adj=Fig_4.3f(n=rep(n_graph,each=17),Sn_m_div = rep(Sn_m_graph,5)))

                        Fig_4.3.site<-data.frame(Sn_m=Sn_m_div,n_graph=paste(n,"years"),Sn_adj=Fig_4.3)


                        Fg4.3<-ggplot(data=Fig_4.3.df,aes(x=Sn_m,y=Sn_adj))+
                                    geom_line(aes(color=n_graph,group=n_graph))+
                                    scale_x_continuous(breaks=seq(0.2,1,0.1))+
                                    scale_y_continuous(breaks=seq(0.2,1.2,0.1))+
                                    scale_color_discrete()+
                                    labs(x="Sn-m/Sn",y="Sn adjustment factor",color="Length of records\n[years]",
                                         title="Figure 4.3. Adjustment of standard deviation of\r
                                         annual series for maximum observed rainfall (Hershfield, 1961)")+
                                    geom_point(data=Fig_4.3.site,aes(x=Sn_m,y=Sn_adj),size=3,color="red")+
                                    geom_text(data=Fig_4.3.site,aes(x=Sn_m,y=Sn_adj,label=n_graph),vjust=-0.5)


                        #Fig 4.4
                        Fig_4.4m<-function(n){(99.7196936977294+(107.866021487998-99.7196936977294)/(1+(n/13.0261452397594)^2.25690664033008))/100}

                        Fig_4.4s<-function(n){(741.591367770118*exp(-n/2.63780903040506)+24.8370732678786*exp(-n/20.9101626977193)+97.9173078131786)/100}

                        Fig_4.4<-c(max(1,Fig_4.4m(n)),max(1,Fig_4.4s(n)))

                        #Figure 4.4 - GGPLOT
                        n_graph<-seq(10,50,1)


                        Fig_4.4.df<-data.frame(n_graph=rep(n_graph,time=2),
                                               Xn_Sn=c(Fig_4.4m(n_graph),Fig_4.4s(n_graph)),
                                               Par=rep(c("Mean","Standard Deviation"),each=41))

                        Fig_4.4.site<-data.frame(n_graph=c(length(X.n),length(X.n)),Xn_Sn=Fig_4.4,Par=c("Mean","Standard Deviation"))

                        library(ggplot2)
                        Fg4.4<-ggplot(data=Fig_4.4.df,aes(x=n_graph,y=Xn_Sn))+
                                    geom_line(aes(color=Par,group=Par))+
                                    scale_x_continuous(breaks=seq(10,100,10))+
                                    scale_y_continuous(breaks=seq(1,1.3,0.05))+
                                    scale_color_discrete()+
                                    labs(x="Length of record\n[years]",y="Adjustment factor",color="Parameters",
                                         title="Figure 4.4. Adjustment of mean and standard deviation\r
                                         of annual series for length of record (Hershfield, 1961)")+
                                    geom_point(data=Fig_4.4.site,aes(x=n,y=Xn_Sn),size=3,color="red")
                        #geom_text(data=Fig_4.4.site,aes(x=n,y=Xn_Sn,label=n_graph),vjust=-0.5)

                        X.n_adj<-X.mean[[1]]*Fig_4.2*Fig_4.4[1]
                        S.n_adj<-X.sd[[1]]*Fig_4.3*Fig_4.4[2]



                        #Fig 4.1

                        Km.f.24<-function(X.mean){
                                    0.0000000001638*X.mean^4 - 0.0000002397*X.mean^3 + 0.0001439*X.mean^2 - 0.05951*X.mean + 19.98}
                        Km.f.6<-function(X.mean){
                                    -0.000000000038701397*X.mean^5 + 0.000000031987514*X.mean^4 - 0.000010135438*X.mean^3 +
                                                0.0015812728*X.mean^2 - 0.16896843*X.mean + 19.951655}
                        Km.f.1<-function(X.mean){
                                    +-0.0000086920029*X.mean^3 + 0.0023070606*X.mean^2 - 0.29365613*X.mean + 19.971686}

                        if (input_hrs==24){Km.f<-Km.f.24}

                        if (input_hrs==6){Km.f<-Km.f.6}

                        if (input_hrs==1){Km.f<-Km.f.1}

                        Km<-Km.f(X.mean[[1]])

                        mamr<-seq(0,600,1)

                        Fig_4.1.df<-data.frame(mamr=rep(mamr,times=3),
                                               rainfall.duration=as.character(rep(c("01","06","24"),each=601)),
                                               Km=c(Km.f.1(mamr),Km.f.6(mamr),Km.f.24(mamr)))

                        Fig_4.1.site<-data.frame(mamr=X.mean[[1]],rainfall.duration=input_hrs,Km=Km)

                        Fg4.1<-ggplot(data=Fig_4.1.df,aes(x=mamr,y=Km))+
                                    geom_line(aes(color=rainfall.duration,group=rainfall.duration))+
                                    scale_x_continuous(breaks=seq(0,600,50))+
                                    scale_y_continuous(limits=c(5,20),breaks=seq(5,20,2.5))+
                                    scale_color_discrete()+
                                    labs(x="Mean Annual Maximum Rainfall [mm]",y="Km",color="Storm Duration\n[hrs]",
                                         title="Figure 4.1. Km as a function of rainfall duration and\r
                                         mean of annual series (Hershfield, 1965)")+
                                    geom_point(data=Fig_4.1.site,aes(x=mamr,y=Km),size=3,color="red")



                        PMP1<-X.n_adj+Km*S.n_adj

                        #Fig 4.5
                        nou.f<-function(number_of_obs_unit){(100.2766207711+(684.0989213113-100.2766207711)/(1+(number_of_obs_unit/0.03150054213)^1.116899589012)
                        )/100}

                        PMP2<-PMP1*nou.f(number_of_obs_unit)

                        nou<-seq(0,24,0.1)

                        Fig_4.5.df<-data.frame(nou=nou,
                                               adjf=nou.f(nou))

                        Fig_4.5.site<-data.frame(nou=number_of_obs_unit,
                                                 adjf=nou.f(number_of_obs_unit))

                        Fg4.5<-ggplot(data=Fig_4.5.df,aes(x=nou,y=adjf))+
                                    geom_line()+
                                    scale_x_continuous(breaks=seq(0,24,2))+
                                    scale_y_continuous(limits=c(1,1.15),breaks=seq(1,1.15,0.02))+
                                    labs(x="Number of Observational Units",y="Adjustment Factor",
                                         title="Figure 4.5. Adjustment of fixed-interval precipitation amounts for\r
                                         number of obsrevational units within the interval (Weiss, 1964)")+
                                    geom_point(data=Fig_4.5.site,aes(x=nou,y=adjf),size=3,color="red")


                        #Fig 4.7
                        area_corr.f.24<-function(area_km){
                                    (-1.13762590487202E-13*area_km^5+3.43734418674257E-10*area_km^4+
                                                 -4.00454902021794E-07*area_km^3+0.000226801496747288*area_km^2+
                                                 -0.0710318092970015*area_km+101.210146730303)/100}
                        area_corr.f.6<-function(area_km){
                                    (+-901.2243442608+(113.7869909952--901.2243442608)/(1+(area_km/1796029203.58)^0.2343007717958))/100}
                        area_corr.f.1<-function(area_km){
                                    (47.13563121645+(126.0011250857-47.13563121645)/(1+(area_km/69.77177029402)^0.5654945864788))/100}

                        if (input_hrs==24){area_corr<-area_corr.f.24(area_km)}

                        if (input_hrs==6){area_corr<-area_corr.f.6(area_km)}

                        if (input_hrs==1){area_corr<-area_corr.f.1(area_km)}

                        area<-seq(0,1000,5)

                        Fig_4.7.df<-data.frame(area=area,
                                               rainfall.duration=as.character(rep(c("01","06","24"),each=201)),
                                               ppm=c(area_corr.f.1(area),area_corr.f.6(area),area_corr.f.24(area)))

                        Fig_4.7.site<-data.frame(area=area_km,rainfall.duration=input_hrs,ppm=area_corr)


                        Fg4.7<-ggplot(data=Fig_4.7.df,aes(x=area,y=ppm))+
                                    geom_line(aes(color=rainfall.duration,group=rainfall.duration))+
                                    scale_x_continuous(breaks=seq(0,1000,100))+
                                    scale_y_continuous(limits=c(0.6,1),breaks=seq(0.6,1,0.05))+
                                    labs(x="Area[km2]",y="Percentage of probable maximum point,\nor 24 km2, rainfall",
                                         title="Figure 4.7. Depth-area, or area-reduction, curves for\r
                                         western United States (United States Weather Bureau, 1960)",
                                         color="Storm Duration\n[hrs]")+
                                    geom_point(data=Fig_4.7.site,aes(x=area,y=ppm),size=3,color="red")


                        PMP3<-PMP2*area_corr

                        correction<-data.frame(Parameter=c("Xn","Xn_m","Xn_m/Xn","Sn_m/Sn","Fig 4.2","Fig 4.3", "Fig 4.4", "Adj.","Correct Value"),
                                               Avg=c(X.mean[[1]],X.mean[[2]],Xn_m_div,NA,Fig_4.2,NA,Fig_4.4[1],Fig_4.2*Fig_4.4[1],Fig_4.2*Fig_4.4[1]*X.mean[[1]]),
                                               StDev=c(X.sd[[1]],X.sd[[2]],NA,Sn_m_div,NA,Fig_4.3,Fig_4.4[2],Fig_4.3*Fig_4.4[2],Fig_4.3*Fig_4.4[2]*X.sd[[1]]))

                        Results<-list(vector,n,correction,Km,PMP1, round(PMP2,0))
                        names(Results)<-c("values","n","correction","Km(Fig 4.1)","PMP without Fig 4.5 correction","PMP without Fig 4.7")

                        suppressWarnings(grid.arrange(Fg4.1,Fg4.2,Fg4.3,Fg4.4,Fg4.5,Fg4.7,ncol=2))


                        Results

            }


}

