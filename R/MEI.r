#Mei Function

#'MEI Index related with El Nino and La Nina influence
#'@export
MEI<-function(){
            library(lubridate)
            library(RCurl)
            library(zoo)
            y1<-getURL("http://www.esrl.noaa.gov/psd/enso/mei/table.html")

            mei.table<-read.table(text=y1,header=TRUE,skip=13,fill=T,stringsAsFactors = FALSE)

            test <- which(grepl(year(now()), mei.table$YEAR))
            current.year <- ifelse(length(test)==0,year(now())-1,year(now()))

            mei.table<-mei.table[c(1:match(current.year,mei.table[,1])),]

            #titles
            names(mei.table)[2:13]<-month.abb
            #numeric
            mei.table<-as.data.frame(sapply(mei.table,as.numeric))
            #row correction
            row.names(mei.table)<-mei.table$YEAR
            #remove the year
            mei.table<-mei.table[-1]


            mei.list<-as.data.frame(t(mei.table))

            mei.list<-unlist(mei.list)

            mei.date<-as.Date(strptime(paste(substr(names(mei.list),start = 1,stop = 4),"-",substr(names(mei.list),start = 5,stop = 10),"-1",sep=""),"%Y-%m-%d"))

            mei.zoo<-zoo(mei.list,mei.date)

            result<-list(mei.table,mei.zoo)

            names(result)<-c("Table","TS")

            result

}
