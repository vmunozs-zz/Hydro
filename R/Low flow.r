#' Calculate low flow for all the gauges at ECflow.db
#'
#' Calculate low flow for all the gauges at ECflow.db
#' @param n number of days for minimum stats.
#' @export
ECf.low<-function(n=7){

        XnQ.monthly<-sapply(X =ECflow.db$daily.ts,FUN = function(x){
                #Accumulated daily value
                Accum.flow.daily<-rollapply(data=x,width=n,FUN=mean, fill=NA, partial=F, align="center")
                #Monthly Synthesis
                Accum.flow.monthly<-daily2monthly.V2(Accum.flow.daily,FUN = min,na.rm = T,max.miss = 5)
                zoo2matrix(Accum.flow.monthly)
        } )

        XnQ.Annual<-sapply(X=XnQ.monthly, FUN=function(x){
                #January to December
                annual<-data.frame(XnQ=rowMins(x[,c(1:12)],),stringsAsFactors = F,row.names =row.names(x) )
                list(annual)
        })

        XnQ.FA<-sapply(X=XnQ.Annual, FUN=function(x){
                #Remove NA and Zeros
                FA.results<-FA(x[!is.na(x)&x>0,])
                list(FA.results$results)
        })

        XnQ.table<-as.data.frame(XnQ.FA,stringsAsFactors = F)


        #Dimensional Table [m3/s]
        XnQ.table<-XnQ.table[-seq(from = 3,to = ncol(XnQ.table),by=2)]

        names(XnQ.table)[1]<-"Prob."

}

#' Calculate low flow for all a time series
#'
#' Calculate low flow for all a time series
#' @param n number of days for minimum stats.
#' @export
ECf.low.ts<-function(ts, n=7){

        library(hydroTSM)
        library(zoo)
        library(matrixStats)

        Accum.flow.daily<-rollapply(data=ts,width=n,FUN=mean, fill=NA, partial=F, align="center")
        #Monthly Synthesis
        Accum.flow.monthly<-daily2monthly.V2(Accum.flow.daily,FUN = min,na.rm = T,max.miss = 5)
        X7Q.monthly<-zoo2matrix(Accum.flow.monthly)



        #January to December
        X7Q.Annual<-data.frame(X7Q=rowMins(X7Q.monthly[,c(1:12)],),
                               stringsAsFactors = F,
                               row.names =row.names(X7Q.monthly))

        #Remove NA and Zeros
        X7Q.FA<-FA(X7Q.Annual)

        #annual result
        X7Q.table.annual<-as.data.frame(X7Q.FA$results,stringsAsFactors = F)
        names(X7Q.table.annual)[2]<-paste("Annual.",names(X7Q.table.annual)[2],sep="")


        #monthly results

        X7Q.monthly<-as.matrix( X7Q.monthly)
        X7Q.monthly.sep<-split(X7Q.monthly,col(X7Q.monthly))

        #Combined
        results.combined<-mapply(FA,X7Q.monthly.sep)

        result.list<-results.combined[4,]

        #Capture de probabiities colum
        result.prob<-result.list[[1]]$Prob

        #Remove extra colum
        FA.results<-lapply(result.list,FUN=function(x){x[,2]})

        result.names<-lapply(result.list,FUN=function(x){names(x)[2]})

        FA.result.df<-data.frame(FA.results)
        names(FA.result.df)<-paste(month.abb,unlist(result.names),sep=".")

        X7Q.table.monthly<-data.frame(result.prob,FA.result.df)


X7Q.table<-cbind(X7Q.table.monthly,X7Q.table.annual)
X7Q.table<-X7Q.table[,-14]
X7Q.table
}


