#GIT HUB

library(Hydro)
ECm.variables(library)

ECm.find(lat=49,lon = -114,n = 10,timeframe = 1)

ECm.capture.all(multicore = T,timeframe = 1)

str(ECm.stations.daily)

load("C:/Users/vmunoz/Dropbox/R Packages/Hydro/EC Daily Canada Data - 20 Stations.RData")

#name of the variables
ECm.variables(db = ECm.stations.daily)

#compile
ECm.prepare(db = ECm.stations.daily,Parameter.name = "Total.Precip..mm.")

ECm.analysis(db = Total.Precip..mm.db,FUN.month = sum, Min.days.annual = 360,
             Max.miss.days.month = 5,output = "Total.Precip..mm.syn",EC = T,
             matrix.start.year = 1980)


ECm.rm(db = "Total.Precip..mm.db",Total.Precip..mm.syn$MWI$MWI.Avg<10,EC=T)

ECm.analysis(db = Total.Precip..mm.db,FUN.month = sum,Min.days.annual = 360,
             Max.miss.days.month = 5,output = "Total.Precip..mm.syn",EC = T,
             matrix.start.year = 1960)


Total.Precip..mm.syn$Max.Daily.table

library(hydroTSM)
hydroTSM::daily2annual()

#Link for the information to be downloaded
ECm.text()

#Link for the climate normals.
#useful to include the climate normals from 1971-200 , 1961-1990.
ECm.text.n()

#get the Information from the web ID
ECm.id()

#Can you also include the climate normals for other 30yrs period?; however, by default should be last one.
ECm.normal()

ECm.capture()



ECm.find(lat=49,lon = -114,n = 1,timeframe = 1)

ECm.capture.all(multicore = T,timeframe = 1)

a<-UC()
str(a)

plot(a$`1018620`,type="b")


str(a$`1018620`)

zoo2matrix(a$`1018620`)

#https://www.ec.gc.ca/dccha-ahccd/default.asp?lang=en&n=9AA530BE-1
ECm.out()

ECm.normal.avail(ECm.normal(1237))

#Flows

#Environnent
ECf.find()


NAf.find(lat = 66,lon = -150,n = 3)

ECf.add(x = ECflow.idx$id,monthly.zero = T)

#Similar to capt
NAf.add(x = NAflow.idx$id,monthly.zero = T)

ECflow.db$

ECf.analysis()

ECf.analysis()

NAflow.syn$mwi.table

NAflow.syn$dwi.table



