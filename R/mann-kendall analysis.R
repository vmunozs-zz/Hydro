#'Mann-Kendall Results from Time series
#'
#'Prepare a Mann-Kendall analysis based on time series.
#'@param ts Zoo time series
#'@param prob Probability to evalution
#'@return return the web link to all the model associated to this assessment report
#'@export
MannKendall.std<-function(ts,prob=0.95){
        
        library("Kendall")
        library("zyp")
        library("mblm")
        library("zoo")
        library("quantreg")
        library("lubridate")
        
        set.seed(1)
        
        if(is.Date(time(ts))==TRUE){
                time_ts<-decimal_date(time(ts))        
        
        ts<-zoo(coredata(ts),time_ts)
        }
        
        
        
        S<-as.numeric(MannKendall(ts)$S)
        VarS<-as.numeric(MannKendall(ts)$varS)
        Tau<-as.numeric(MannKendall(ts)$tau)
        p_value<-as.numeric(MannKendall(ts)$sl)/2
        p_crit<-qnorm(prob,mean = 0,sd = 1,lower.tail=T)
        
        
        #Analisis
        x=time(ts)
        y=coredata(ts)
        
        yuepilon<-zyp.yuepilon2(y, x,  conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)

        zhang<-zyp.zhang2(y, x,  conf.intervals=TRUE, preserve.range.for.sig.test=TRUE)
        
        #Theil zedn
        theil.sen2<-mblm(y~x)
        
        #Quantile regression
        quantile<-rq(y~x,0.5,method = "fn")
        
        #Linear Regression
        LinearM<-lm(y~x)
        
        
        ###################SEN SLOPE########################
        #Z definition
        sen.aux<-function(S,VarS){
                   
                if(S>0){
                        Z<-(S-1)/sqrt(VarS)
                } else{
                        if(S<0) {
                                Z<-(S+1)/sqrt(VarS)
                        }else{ Z<-0 }
                }
                
                #Trend definition
                if(S>10){   text<-"Upward Trend, "
                } else {
                        if(S< -10){text<-"Downward Trend, "
                        } else {
                                text<-"No Trend, "
                        }
                }
                
                ret<-list(Z,text)
                
                names(ret)<-c("Z","text")
                
                ret
                
        }
        sen.1<-sen.aux(S,VarS)
        sen.2<-sen.aux(as.numeric(zhang["S"]),as.numeric(zhang["varS"]))
        sen.3<-sen.aux(as.numeric(yuepilon["S"]),as.numeric(yuepilon["varS"]))
        
        #Presenting comments
        
        text1<- if(sen.1$Z>p_crit| sen.1$Z< -p_crit){paste(sen.1$text,prob*100,"% of significance",sep="")}else{"There is no significant"}
        
        text2<- if(sen.2$Z>p_crit | sen.2$Z< -p_crit){paste(sen.2$text, prob*100,"% of significance",sep="")}else{"There is no significant"}
        
        text3<- if(sen.3$Z>p_crit | sen.3$Z< -p_crit){paste(sen.2$text,prob*100,"% of significance",sep="")}else{"There is no significant"}
        
        
        
        #Theil - Sen analysis
        
        
        MK.results<-data.frame(Method=c("Ordinary\nleast squares",
                                        "Quartile\nRegression",
                                        "MK(1945) &\nTheil-Sen(1968)",
                                        "Prewhit. MK,\nZhang(2001)",
                                        "Prewhit. MK,\nYue & Pilon(2002)"),
                               S=c(NA,NA,S,as.numeric(zhang["S"]),as.numeric(yuepilon["S"])),
                               Z=c(NA,NA,sen.1$Z,sen.2$Z,sen.3$Z),
                               tau=c(NA,NA,Tau,zhang["tau"],yuepilon["tau"]),
                               #p_2_side=c(NA,NA,p_value*2,zhang["sig"]*2,yuepilon["sig"]*2),
                               p_value=c(anova(LinearM)$'Pr(>F)'[1],summary.rq(quantile,se="boot")$coefficients[2,4],p_value,zhang["sig"],yuepilon["sig"]),
                               p_crit=c(rep(p_crit,5)),
                               slope=c(LinearM$coefficients[2],
                                       quantile$coefficients[2],
                                       theil.sen2$coefficients[2],
                                       zhang["trend"],
                                       yuepilon["trend"]),
                               intercept=c(LinearM$coefficients[1],
                                           quantile$coefficients[1],
                                           theil.sen2$coefficients[1],
                                           zhang["intercept"],
                                           yuepilon["intercept"]),
                               r2=c(summary(LinearM)$r.squared,
                                    NA,
                                    NA,
                                    zhang["linear"],
                                    yuepilon["linear"]),
                               Comment=c(NA,NA,text1,text2,text3),
                               Significance=c( 1-anova(LinearM)$'Pr(>F)'[1],
                                               1-summary.rq(quantile,se="boot")$coefficients[2,4],
                                              if(sen.1$Z>0){pnorm(sen.1$Z,mean = 0,sd = 1,lower.tail=T)}else{pnorm(sen.1$Z,mean = 0,sd = 1,lower.tail=F)},
                                              if(sen.2$Z>0){pnorm(sen.1$Z,mean = 0,sd = 1,lower.tail=T)}else{pnorm(sen.2$Z,mean = 0,sd = 1,lower.tail=F)},
                                              if(sen.3$Z>0){pnorm(sen.1$Z,mean = 0,sd = 1,lower.tail=T)}else{pnorm(sen.3$Z,mean = 0,sd = 1,lower.tail=F)}))
        
        1-summary.rq(quantile,se="boot")$coefficients[2,4]
        
        #names(results)<-c("S","Stand.S","tau","p_2_side","p_value","p_crit","Comment")
        #resultsas.data.frame(results,stringsAsFactors = F)
        MK.results
}

#'Mann-Kendall Evaluation for Climate Change analysis
#'
#'Prepare a Mann-Kendall analysis based on time series.
#'@param ts Zoo time series
#'@param prob Probability to evalution
#'@return return a table with the parameter change
#'@export
MannKendall.eval<-function(ts,prob=0.95, Baseline.start, Baseline.end, Analysis.start, Analysis.end){
        MK.results<-MannKendall.std(ts,prob)        
        
        Evaluation.X1<-((MK.results$slope*Analysis.end+MK.results$intercept)+(MK.results$slope*Analysis.start+MK.results$intercept))/2
        Evaluation.X0<-((MK.results$slope*Baseline.end+MK.results$intercept)+(MK.results$slope*Baseline.start+MK.results$intercept))/2
        
        MK.eval<-data.frame(Method=MK.results$Method,
                            Baseline=Evaluation.X0,
                            Projection=Evaluation.X1,
                            Evaluation=100*(Evaluation.X1-Evaluation.X0)/Evaluation.X0,
                            stringsAsFactors = F)     
        MK.eval
}

#'Sen's slope algorith based on Yue-Philon
#'
#'Sen's slope algorith based on Yue-Philon
#'@export
#From ZYP library, just modified to optain S and varS
zyp.yuepilon2<-function (y, x = 1:length(y), conf.intervals = TRUE, preserve.range.for.sig.test = TRUE) 
{
        dat <- as.numeric(as.vector(y))
        if (is.logical(x)) 
                stop("x cannot be of type 'logical' (perhaps you meant to specify conf.intervals?)")
        n <- length(dat)
        t <- x
        t.prime <- t[1:(n - 1)]
        y <- dat
        ret <- c(lbound = NA, trend = NA, trendp = NA, ubound = NA, 
                 tau = NA, sig = NA, nruns = NA, autocor = NA, valid_frac = NA, 
                 linear = NA, intercept = NA,S=NA,varS=NA)
        dmap <- which(!is.na(y))
        ynm <- as.numeric(y[dmap])
        tnm <- as.numeric(t[dmap])
        if (length(dmap) <= 3 | length(which(ynm != 0)) < 3 | length(dmap)/n < 
                    0.1) {
                return(ret)
        }
        sen <- zyp.sen(ynm ~ tnm)
        trend <- sen$coefficients[2]
        xt.prime <- dat[1:n] - trend * t
        ac <- acf(xt.prime, lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2]
        if (is.na(ac)) {
                return(ret)
        }
        yt.prime <- ifelse(rep(preserve.range.for.sig.test, n - 
                                       1), (xt.prime[2:n] - ac * xt.prime[1:(n - 1)])/(1 - 
                                                                                               ac), xt.prime[2:n] - ac * xt.prime[1:(n - 1)])
        yt <- yt.prime[1:(n - 1)] + trend * t.prime
        dmap.prime <- which(!is.na(yt))
        ytnm <- as.numeric(yt[dmap.prime])
        Kend <- Kendall(t.prime[dmap.prime], ytnm)
        tau <- Kend[1]
        Bsig <- Kend[2]
        S<-Kend[3]
        varS<-Kend[5]
        if (conf.intervals) {
                ci <- confint(sen)
        }
        else {
                ci <- matrix(rep(NA, 4), nrow = 2, ncol = 2)
        }
        ret <- c(lbound = as.numeric(ci[2, 1]), trend = as.numeric(trend), 
                 trendp = as.numeric(trend) * n, ubound = as.numeric(ci[2, 
                                                                        2]), tau = as.numeric(tau), sig = as.numeric(Bsig), 
                 nruns = 1, autocor = as.numeric(ac), valid_frac = as.numeric(length(dmap)/length(y)), 
                 linear = as.numeric(lm(dat ~ t)$coefficients[2]), intercept = as.numeric(sen$coefficients[1]),
                 S=as.numeric(S),varS=as.numeric(varS))
        return(ret)
}

#'Sen's slope algorith based on Zhang
#'
#'Sen's slope algorith based on Zhang
#'@export
#From ZYP library, just modified to optain S and varS
zyp.zhang2<-function (y, x = 1:length(y), conf.intervals = TRUE, preserve.range.for.sig.test = TRUE) 
{
        data <- as.numeric(as.vector(y))
        if (is.logical(x)) 
                stop("x cannot be of type 'logical' (perhaps you meant to specify conf.intervals?)")
        n <- length(data)
        t <- x
        ret <- c(lbound = NA, trend = NA, trendp = NA, ubound = NA, 
                 tau = NA, sig = NA, nruns = NA, autocor = NA, valid_frac = NA, 
                 linear = NA, intercept = NA,S=NA,varS=NA)
        c <- acf(data, lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2]
        if (is.na(c)) {
                return(ret)
        }
        if (c < 0.05) {
                y <- data
                yt <- t
        }
        else {
                y <- ifelse(rep(preserve.range.for.sig.test, n - 1), 
                            (data[2:n] - c * data[1:(n - 1)])/(1 - c), data[2:n] - 
                                    c * data[1:(n - 1)])
                yt <- t[1:(n - 1)]
        }
        dmap <- which(!is.na(y))
        ynm <- as.numeric(y[dmap])
        ytnm <- as.numeric(yt[dmap])
        if (length(dmap) <= 3 | length(which(ynm != 0)) < 3 | length(dmap)/n < 
                    0.1) {
                return(ret)
        }
        sen <- zyp.sen(ynm ~ ytnm)
        trend <- sen$coefficients[2]
        k <- 1
        c0 <- c
        if (c >= 0.05) {
                trend0 <- trend
                while (k < 500) {
                        x <- data[1:n] - trend * t
                        c <- acf(x, lag.max = 1, plot = FALSE, na.action = na.pass)$acf[2]
                        if (c < 0.05 && abs(c - c0) <= 1e-04) {
                                break
                        }
                        y <- (data[2:n] - c * data[1:(n - 1)])/(1 - c)
                        dmap <- which(!is.na(y))
                        ynm <- as.numeric(y[dmap])
                        ytnm <- as.numeric(yt[dmap])
                        sen <- zyp.sen(ynm ~ ytnm)
                        trend <- sen$coefficients[2]
                        k <- k + 1
                        tpdiff <- abs((trend - trend0)/trend)
                        if (!is.na(tpdiff) && tpdiff <= 0.001 && abs(c - 
                                                                             c0) <= 1e-04) {
                                break
                        }
                        trend0 <- trend
                        c0 <- c
                }
        }
        Kend <- Kendall(ytnm, ynm)
        tau <- Kend[1]
        Bsig <- Kend[2]
        S<-Kend[3]
        varS<-Kend[5]
        if (conf.intervals) {
                ci <- confint(sen)
        }
        else {
                ci <- matrix(rep(NA, 4), nrow = 2, ncol = 2)
        }
        ret <- c(lbound = as.numeric(ci[2, 1]), trend = as.numeric(trend), 
                 trendp = (as.numeric(trend) * n), ubound = as.numeric(ci[2, 
                                                                          2]), tau = as.numeric(tau), sig = as.numeric(Bsig), 
                 nruns = as.numeric(k), autocor = as.numeric(c0), valid_frac = as.numeric(length(dmap)/length(y)), 
                 linear = as.numeric(lm(data ~ t)$coefficients[2]), intercept = as.numeric(sen$coefficients[1]),
                 S=as.numeric(S),varS=as.numeric(varS))
        return(ret)
}

