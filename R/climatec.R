
# Libraries ---------------------------------------------------------------

#Rcurl is a library related with internet conections
library("RCurl")

#XML allows to interpret the results from a internet answer
library("XML")

#String formating
library("stringr")

# Text Preparation --------------------------------------------------------

#'Web link for Assesment report  (AR 1 to AR 4)
#'
#'Prepare the web link to call for the models associated to each assessment report.
#'@param assessment A numeric value associated to the assessment report
#'@return return the web link to all the model associated to this assessment report
#'@export
ar.text<-function(assessment=2){
        paste("http://www.cccsn.ec.gc.ca/tools/dd-gcm/gcm-wb.pl?f=populate_model&",
              "args=",assessment,sep="")
}

#'Web link for Experiment (AR 1 to AR 4)
#'
#'Prepare the web link to call for the experiment associated to each model and assessment report.
#'@param row Row value for the Index table associated to AR1 to AR4
#'@return return the web link to the experiment associated to the DB row
#'@export

ex.text<-function(row=1){
        table<-Table.AR
        if(row>nrow(table)){NA
        }else{
                paste("http://www.cccsn.ec.gc.ca/tools/dd-gcm/gcm-wb.pl?f=populate_experiment",
                      "&args=",table$Assessment[row],
                      "%2C",table$Option.1[row],
                      "%2C",table$Option.2[row],sep="")
        }
}
#'Prepare the web link to call for the variables associated to each model and assessment report.
#'@export
var.text<-function(row=1){
        table<-Table.AR.EX
        if(row>nrow(table)){NA
        }else{
                paste("http://www.cccsn.ec.gc.ca/tools/dd-gcm/gcm-wb.pl?f=populate_variable",
                      "&args=",table$Assessment[row],
                      "%2C",table$Option.1[row],
                      "%2C",table$Option.2[row],
                      "%2C",table$Option.3[row],sep="")
        }
}



time.text<-function(row=1){
        table<-Table.AR.EX.VAR
        if(row>nrow(table)){NA
        }else{
                paste("http://www.cccsn.ec.gc.ca/tools/years-validate/dd-gcm-wb.pl?f=produce_year_list",
                      "&args=",table$Assessment[row],
                      "%2C",table$Option.1[row],
                      "%2C",table$Option.2[row],
                      "%2C",table$Option.3[row],
                      "%2C",table$Option.4[row],sep="")
        }
}



# Table Preparation -------------------------------------------------------

#'Construct a table with models for the given assessment report
#'@export
ar.table<-function(assessment=2){

        www.table<-getURL(url=ar.text(assessment))
        #transform parse the xml in a table
        xml.table<-htmlTreeParse(www.table, error=function(...){})
        #amount of element of a table
        n.elements<-length(xml.table$children$html[1]$body)
        #extract a section of the table to work
        extract.table<-xml.table$children$html[1]$body
        synthesis.table<-as.data.frame(NULL)

        for (i in 1:n.elements) {

                #assessment
                synthesis.table[i,1]<-assessment
                #experiment value
                options.values<-unlist(strsplit(unlist(extract.table[i]$option)[2],","))

                synthesis.table[i,2]<-as.numeric(options.values[1])

                synthesis.table[i,3]<-as.numeric(options.values[2])
                #experiment name
                synthesis.table[i,4]<-unlist(extract.table[i]$option)[4]

        }
        names(synthesis.table)<-c("Assessment","Option.1","Option.2","Model")
        synthesis.table
}

ex.table<-function(row=6){

        table<-Table.AR
        www.table<-getURL(url=ex.text(row))
        #transform parse the xml in a table
        xml.table<-htmlTreeParse(www.table, error=function(...){})
        #amount of element of a table
        n.elements<-length(xml.table$children$html[1]$body)
        #extract a section of the table to work
        extract.table<-xml.table$children$html[1]$body
        synthesis.table<-as.data.frame(NULL)

        for (i in 1:n.elements) {

                #experiment value
                synthesis.table[i,1]<-table$Assessment[row]

                synthesis.table[i,2]<-table$Option.1[row]

                synthesis.table[i,3]<-table$Option.2[row]

                synthesis.table[i,4]<-as.numeric(unlist(extract.table[i]$option)[2])

                synthesis.table[i,5]<-table$Model[row]

                #experiment name
                synthesis.table[i,6]<-as.character(unlist(extract.table[i]$option)[4])

        }
        names(synthesis.table)<-c("Assessment","Option.1","Option.2","Option.3","Model","Experiment")
        synthesis.table
}


var.table<-function(row=6){

        table<-Table.AR.EX
        www.table<-getURL(url=var.text(row))
        #transform parse the xml in a table
        xml.table<-htmlTreeParse(www.table, error=function(...){})
        #amount of element of a table
        n.elements<-length(xml.table$children$html[1]$body)
        #extract a section of the table to work
        extract.table<-xml.table$children$html[1]$body
        synthesis.table<-as.data.frame(NULL)

        for (i in 1:n.elements) {

                #experiment value
                synthesis.table[i,1]<-table$Assessment[row]

                synthesis.table[i,2]<-table$Option.1[row]

                synthesis.table[i,3]<-table$Option.2[row]

                synthesis.table[i,4]<-table$Option.3[row]

                synthesis.table[i,5]<-as.numeric(unlist(extract.table[i]$option)[2])

                synthesis.table[i,6]<-table$Model[row]

                synthesis.table[i,7]<-table$Experiment[row]

                #experiment name
                synthesis.table[i,8]<-as.character(unlist(extract.table[i]$option)[4])

        }
        names(synthesis.table)<-c("Assessment","Option.1","Option.2","Option.3", "Option.4","Model","Experiment","Variable")
        synthesis.table
}



time.table<-function(row=6){

        table<-Table.AR.EX.VAR
        www.table<-getURL(url=time.text(row))

        #transform parse the xml in a table
        xml.table<-unlist(strsplit(str_replace_all(www.table, "[\r\n]" , ""),","))
        synthesis.table<-as.data.frame(NULL)

        #experiment value
        synthesis.table[1,1]<-table$Assessment[row]
        synthesis.table[1,2]<-table$Option.1[row]
        synthesis.table[1,3]<-table$Option.2[row]
        synthesis.table[1,4]<-table$Option.3[row]
        synthesis.table[1,5]<-table$Option.4[row]
        synthesis.table[1,6]<-xml.table[1]
        synthesis.table[1,7]<-xml.table[2]
        synthesis.table[1,8]<-xml.table[3]
        synthesis.table[1,9]<-table$Model[row]
        synthesis.table[1,10]<-table$Experiment[row]
        #experiment name
        synthesis.table[1,11]<-table$Variable[row]

        names(synthesis.table)<-c("Assessment","Option.1","Option.2","Option.3", "Option.4","Date.1","Date.2","Date.3",
                                  "Model","Experiment","Variable")

        synthesis.table
}


# Table compilation -------------------------------------------------------


#' Compile a table with all the assesment reports
#'@export
ar.table.comp<-function(){
        Table.AR<-ar.table(1)

        for (i in 2:4){
                Table.AR<-rbind(Table.AR,ar.table(i))
        }
        Table.AR<<-Table.AR

}

#' Compile a table with all the assesment reports
#'@export
ex.table.comp<-function(){
        Table.AR.EX<-ex.table(1)

        for (i in 2:nrow(Table.AR)){

                Table.AR.EX<-rbind(Table.AR.EX,ex.table(i))
        }
        Table.AR.EX<<-Table.AR.EX

}

#'Compile a table with all the variables
#'@export
var.table.comp<-function(){
        Table.AR.EX.VAR<-var.table(1)

        for (i in 2:nrow(Table.AR.EX)){

                Table.AR.EX.VAR<-rbind(Table.AR.EX.VAR,var.table(i))
        }
        Table.AR.EX.VAR<<-Table.AR.EX.VAR

}

#'Compile a table with all the time ranges
#'@export
time.table.comp<-function(){
        AR1_4.Index<-time.table(1)

        for (i in 2:nrow(Table.AR.EX.VAR)){

                AR1_4.Index<-rbind(AR1_4.Index,time.table(i))
        }
        AR1_4.Index<<-AR1_4.Index

}


# Index preparation -------------------------------------------------------

#'Preparation of the index file
#'@export
AR1_4.Index.Table<-function(){

        #INDEX List Preparation

        #prepare a table with all the model for all the assesment reports 1 - 4
        ar.table.comp()

        #prepare a table with all the experiment associated to the models.
        ex.table.comp()

        #prepare a table with all the variables associated to the experiments and models.
        var.table.comp()

        #prepare a table with all the variables associated to all the time frames, experiments, models and assesment reports.
        time.table.comp()

        write.csv(AR1_4.Index,"AR1_4.Index.csv")
        save(AR1_4.Index,file="AR1_4.Index.RData",ascii=FALSE)

}
#'Function which read the information from EC based on defined variables
#'@export
cc.text<-function(assessment=2,model.1=23,model.2=1,experiment=16,variable=11,basef=1961,baset=1990,lat=52.70,long=-95.25){

        paste("http://www.cccsn.ec.gc.ca/tools/dd-gcm/gcm-wb.pl",
              "?assessment=",assessment,
              "&model=",model.1,"%2C",model.2,
              "&experiment=",experiment,
              "&variable=",variable,
              "&timeofyear=all",
              #"&baseline=",baseline,
              "&basef=",basef,
              "&baset=",baset,
              "&ptype=on",
              "&lat1_out=",lat,
              "&lng1_out=",long,
              "&lat2_out=",
              "&lng2_out=",
              "&output_type=raw-csv",
              "&get_boxes=Retrieve",sep="")

}


# List Preparation and graph by parameter and years----------------------------------------------

#'Parameter setup for AR 1 to 4
#'@export
AR1_4.par.setup<-function(Parameter=61){

        sel.table<-AR1_4.Index[AR1_4.Index$Option.4==Parameter,]
        n.sel.table<-nrow(sel.table)

        #answer a dataframe for the amount of year of one experiment row from the data list
        n.years<-function(i)        {
                Exp<-sel.table[i,]

                Date1.Var<-if(!is.na(Exp$Date.1)){
                        Date1.aux<-as.numeric(unlist(strsplit(Exp$Date.1,"-")))
                        seq(from = Date1.aux[1],to = Date1.aux[2])
                }

                Date2.Var<-if(!is.na(Exp$Date.2)){
                        Date2.aux<-as.numeric(unlist(strsplit(Exp$Date.2,"-")))
                        seq(from = Date2.aux[1],to = Date2.aux[2])
                }

                Date3.Var<-if(!is.na(Exp$Date.3)){
                        Date3.aux<-as.numeric(unlist(strsplit(Exp$Date.3,"-")))
                        seq(from = Date3.aux[1],to = Date3.aux[2])
                }

                a<-data.frame(Year=c(Date1.Var,Date2.Var,Date3.Var),1)

                names(a)<-c("Year",paste("ID.",i,sep=""))

                a
        }

        library("hydroTSM")

        year.table<-n.years(1)

        for (i in 2:n.sel.table){

                year.table<-merge(year.table,n.years(i),all=TRUE)

        }

        #include rownames
        rownames(year.table)<-year.table$Year

        #include colnames
        colnames(year.table)<-c("Year",paste("AR",sel.table$Assessment,"-",sel.table$Model,sel.table$Experiment))

        #replace NA by zero
        year.table[is.na(year.table)]<-0

        #prepare table. Define table from 1960 to 2200
        matrix.table<-year.table[1:141,2:n.sel.table]

        AR1_4.Sel.Exp<<-sel.table

        #At the table is remove the information of the year; just because it is included in the row name
        AR1_4.Avail.Years<<-year.table[-1]

        matrixplot(matrix.table,ColorRamp ="Days", aspect="fill",main = sel.table$Variable[1])

}
#'Year setup for AR 1 to 4.
#'@export
AR1_4.year.setup<-function(Baseline.start=1960,Baseline.end=1990,Analysis.start=2041,Analysis.end=2070){

        #Auxiliar variable definition
        AR1_4.Avail.Years.temp<-AR1_4.Avail.Years

        #Year denition
        Baseline.start.row<-sum(rownames(AR1_4.Avail.Years.temp)<=Baseline.start)
        Baseline.end.row<-sum(rownames(AR1_4.Avail.Years.temp)<=Baseline.end)

        Analysis.start.row<-sum(rownames(AR1_4.Avail.Years.temp)<=Analysis.start)
        Analysis.end.row<-sum(rownames(AR1_4.Avail.Years.temp)<=Analysis.end)

        #replace NA by zero
        AR1_4.Avail.Years.temp[AR1_4.Avail.Years.temp==0]<-NA

        #Replace name for ID
        colnames(AR1_4.Avail.Years.temp)<-c(rownames(AR1_4.Sel.Exp))

        #Selected just the experiment, which have information in that range.

        AR1_4.Avail.Years.TC<-AR1_4.Avail.Years.temp[,!colSums(is.na(AR1_4.Avail.Years.temp[c(Baseline.start.row:Baseline.end.row,Analysis.start.row:Analysis.end.row),]))]


        AR1_4.Sel.Exp.TC<-AR1_4.Sel.Exp[colnames(AR1_4.Avail.Years.TC),]

        AR1_4.Avail.Years<<-AR1_4.Avail.Years.TC

        AR1_4.Sel.Exp<<-AR1_4.Sel.Exp.TC
}


# Get the information from row i from AR1_4.Sel.Exp ------------------------

#'Get the information from EC server based Latitude and Longitude for one index row.
#'@export
AR1_4.get<-function(Latitude, Longitude, i=1){
        library("RCurl")
       #i<-1
        #Baseline info
        # B for Baseline and A for Analysis
        file1<-tempfile()
        file2<-tempfile()


        EC.cc.link.B<-cc.text(assessment = AR1_4.Sel.Exp$Assessment[i],
                              model.1 = AR1_4.Sel.Exp$Option.1[i],
                              model.2 = AR1_4.Sel.Exp$Option.2[i],
                              experiment = AR1_4.Sel.Exp$Option.3[i],
                              #variable=61,
                              variable = AR1_4.Sel.Exp$Option.4[i],
                              basef = Baseline.start,
                              baset = Baseline.end,
                              lat = Latitude,
                              long = Longitude)

        EC.cc.web.B<-getURL(url=EC.cc.link.B,async=F)
        write.csv(EC.cc.web.B,file2)

        # Check if there is information table with information  5616 bytes, without 1056 bytes. with just annuals 1264 bytes

        file.type<-if(as.numeric(object.size(EC.cc.web.B))>5000) {
                2
        } else{
                if(as.numeric(object.size(EC.cc.web.B))>500) {
                        1
                } else{
                        0}
        }

        #Check if the file was not possible to be created.
        pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
        plain.text <- gsub(pattern, "\\1", EC.cc.web.B)

        if(substr(plain.text,5,45)=="Your requested output could not be create"){file.type=0}

        #Decision result
        if(file.type==2|file.type==1){

                EC.cc.table.B<-read.csv(file=file2,skip=7,nrows=Baseline.end-Baseline.start+1,as.is=T,head=T,sep=",")

                #in case of annual records just correct the table tittle
                if(file.type==1){names(EC.cc.table.B)<-c("lat","lon","year","annual")}

                if(Parameter==61){EC.cc.table.B$annual<-EC.cc.table.B$annual*365.25} #if Pp =>annual from mm/day to mm/year

                EC.cc.table.B<-EC.cc.table.B[,c(-1:-2,-16:-19)]
                attr(EC.cc.table.B,"ID")<-rownames(AR1_4.Sel.Exp)[i]
                attr(EC.cc.table.B,"Latitude")<-Latitude
                attr(EC.cc.table.B,"Longitude")<-Longitude
                attr(EC.cc.table.B,"Model")<-AR1_4.Sel.Exp$Model[i]
                attr(EC.cc.table.B,"Experiment")<-AR1_4.Sel.Exp$Experiment[i]
                attr(EC.cc.table.B,"Years")<-paste(Baseline.start,Baseline.end,sep="-")
                attr(EC.cc.table.B,"Mean")<-mean(EC.cc.table.B$annual)
                attr(EC.cc.table.B,"St.Dev")<-sd(EC.cc.table.B$annual)

                print(paste(AR1_4.Sel.Exp$Model[i],AR1_4.Sel.Exp$Experiment[i],AR1_4.Sel.Exp$Variable[i],"(",Baseline.start,"-",Baseline.end,")"))

                file.remove(file2)

                #Analysis info
                EC.cc.link.A<-cc.text(assessment = AR1_4.Sel.Exp$Assessment[i],
                                      model.1 = AR1_4.Sel.Exp$Option.1[i],
                                      model.2 = AR1_4.Sel.Exp$Option.2[i],
                                      experiment = AR1_4.Sel.Exp$Option.3[i],
                                      variable = AR1_4.Sel.Exp$Option.4[i],
                                      basef = Analysis.start,
                                      baset = Analysis.end,
                                      lat = Latitude,
                                      long = Longitude)

                EC.cc.web.A<-getURL(url=EC.cc.link.A,async=F)

                write.csv(EC.cc.web.A,file1)
                EC.cc.table.A<-read.csv(file=file1,skip=7,nrows=Analysis.end-Analysis.start+1,as.is=T,head=T,sep=",")

                #in case of annual records just correct the table tittle
                if(file.type==1){names(EC.cc.table.A)<-c("lat","lon","year","annual")}

                if(Parameter==61){EC.cc.table.A$annual<-EC.cc.table.A$annual*365.25} #if Pp =>annual from mm/day to mm/year

                EC.cc.table.A<-EC.cc.table.A[,c(-1:-2,-16:-19)]
                attr(EC.cc.table.A,"ID")<-rownames(AR1_4.Sel.Exp)[i]
                attr(EC.cc.table.A,"Latitude")<-Latitude
                attr(EC.cc.table.A,"Longitude")<-Longitude
                attr(EC.cc.table.A,"Model")<-AR1_4.Sel.Exp$Model[i]
                attr(EC.cc.table.A,"Experiment")<-AR1_4.Sel.Exp$Experiment[i]
                attr(EC.cc.table.A,"Years")<-paste(Analysis.start,Analysis.end,sep="-")
                attr(EC.cc.table.A,"Mean")<-mean(EC.cc.table.A$annual)
                attr(EC.cc.table.A,"St.Dev")<-sd(EC.cc.table.A$annual)

                print(paste(AR1_4.Sel.Exp$Model[i],AR1_4.Sel.Exp$Experiment[i],AR1_4.Sel.Exp$Variable[i],"(",Analysis.start,"-",Analysis.end,")"))

                file.remove(file1)
        } else {
                EC.cc.table.B<-NA
                EC.cc.table.A<-NA

        }

        #Return the info
        result<-list(Baseline=EC.cc.table.B,Analysis=EC.cc.table.A)

}
#'Get ALL the information from EC server based Latitude and Longitude.
#'@export
AR1_4.get.all<-function(Latitude, Longitude){
        library(parallel)

        cl<-makeCluster(detectCores())

        clusterExport(cl,varlist = list("Longitude","Latitude","AR1_4.Sel.Exp",
                                        "Baseline.start","Baseline.end","Parameter",
                                        "Analysis.start","Analysis.end"))

        AR1_4.Raw_Results<-parLapply (cl,1:nrow(AR1_4.Sel.Exp),fun=function(i){
                print(paste(i,"/",nrow(AR1_4.Sel.Exp)))

                Hydro::AR1_4.get(Latitude, Longitude,i)


        })

        stopCluster(cl)

        names(AR1_4.Raw_Results)<-make.names(rownames(AR1_4.Sel.Exp))

        AR1_4.Raw_Results<<-AR1_4.Raw_Results

        save(AR1_4.Raw_Results, file="AR1_4.Raw_Results.RData",ascii=FALSE)

}


#'Process the results from the files from AR 1 to AR 4
#'@export
AR1_4.get.results<-function(){

        AR1_4.Results<-data.frame(ID=rownames(AR1_4.Sel.Exp),
                                  Assesment=paste("AR",AR1_4.Sel.Exp$Assessment,sep=""),
                                  Experiment=paste("AR",AR1_4.Sel.Exp$Assessment,AR1_4.Sel.Exp$Model,AR1_4.Sel.Exp$Experiment),
                                  Baseline.Mean=rep(NA,times = nrow(AR1_4.Sel.Exp)),
                                  Analysis.Mean=rep(NA,times = nrow(AR1_4.Sel.Exp)),
                                  Change.Rate=rep(NA,times = nrow(AR1_4.Sel.Exp)),
                                  stringsAsFactors = F)

        for (j in 1:nrow(AR1_4.Sel.Exp)){

                i<-AR1_4.Raw_Results[j]


                AR1_4.Results$Baseline.Mean[j]<-if(!is.null(ncol((i)[[1]][[1]]))){
                        attributes(i[[1]][[1]])$Mean
                } else{
                        NA
                }

                AR1_4.Results$Analysis.Mean[j]<-if(!is.null(ncol((i)[[1]][[1]]))){
                        attributes(i[[1]][[2]])$Mean
                } else{
                        NA
                }


        }

        if(sum(Parameter==c(11,15,16,231))>=1){
                AR1_4.Results$Change.Rate<-100*((AR1_4.Results$Analysis.Mean+273.15)-(AR1_4.Results$Baseline.Mean+273.15))/(AR1_4.Results$Baseline.Mean+273.15)

        }else{
                AR1_4.Results$Change.Rate<-100*(AR1_4.Results$Analysis.Mean-AR1_4.Results$Baseline.Mean)/AR1_4.Results$Baseline.Mean
        }

        AR1_4.Results<-AR1_4.Results[complete.cases(AR1_4.Results),]
        AR1_4.Results<<-AR1_4.Results
}

#'Plot the information from the ARs.
#'@export
AR.plot<-function(Reanalysis.figure, AR.Results=AR1_5.Results, pdf.out=TRUE) {
        #Libraries
        library("ggplot2")
        library("grid")
        library("gridExtra")

        if(exists("AR1_5.Results")==FALSE){AR.Results=AR1_4.Results}


        options(encoding = "latin1")

        #Matrix combined
        AR.Results.combined<-AR.Results
        AR.Results.combined$Assesment="ALL"
        AR.Results.combined<-rbind(AR.Results,AR.Results.combined)

        #data frame with assessment for all

        AR.Results2<-AR.Results
        AR.Results2$Assesment<-"ALL"


        units1<-Reanalysis.figure$units

        #Parameter text
        Parameter.df<-data.frame(Value=c(11,15,16,61,52,51,2,32,34,33,116,231,236,230,239,234,232,233,237,235,238),
                                 text=c("Air Temperature - Mean (2m)","Air Temperature - Mean Max (2m)",
                                        "Air Temperature - Mean Min (2m)","Precipitation - Total","Humidity - Relative (2m)",
                                        "Humidity - Specific (2m)","Sea Level Pressure","Wind Speed - Mean (10m)",
                                        "Wind Speed - Meridional (10m)","Wind Speed - Zonal (10m)","Surface Downwelling Shortwave Radiation",
                                        "Air Temperature - Extreme Range (2m)","Days - Consecutive Dry","Days - Frost",
                                        "Fraction of Time - Annual Precipitation > 95th Percentile",
                                        "Fraction of Time - Daily T-min > 90th Percentile","Growing Season Length","Heat Wave Duration Index",
                                        "Precipitation - 5 Day Max","Precipitation - Days > 10mm/d","Precipitation - Simple Daily Intensity Index"),
                                 stringsAsFactors = F)
        Parameter.text<-Parameter.df$text[Parameter.df$Value==Parameter]

        #Matching colours for figure 1 in ggplot2
        gg_color_hue <- function(n) {
                hues = seq(15, 375, length=n+1)
                hcl(h=hues, l=65, c=100)[1:n]
        }

        Amount.colors<-length(table(AR.Results.combined$Assesment))
        cols = gg_color_hue(Amount.colors)


        #Figure 1 - Compiled Climate Change Scenarios
        Fig1<-ggplot(AR.Results, aes(x=Experiment,y=Change.Rate,colour=Assesment))+
                geom_bar(stat="identity")+
                theme(axis.text.x=element_text(angle=90,hjust=1))+
                ylab("Change with respect to Baseline [%]")+
                xlab("")+
                ggtitle(paste("Climate parameter: ",Parameter.text,"\n",
                              "Latitude: ",Latitude,", Longitude: ", Longitude,"\n",
                              "Baseline: ",Baseline.start," - ",Baseline.end,
                              " / Projection: ",Analysis.start," - ",Analysis.end,"\n", sep=""))+
                theme(plot.title = element_text(lineheight=.8, face="bold",size=rel(1.5)),
                      axis.text.x = element_text(size=rel(0.8),face="plain"))+
                scale_colour_manual(values=cols[2:Amount.colors],name="Assessment\n Report")
        #Fig1
        #FIGURE 2 - Violin Figure
        Fig2<-ggplot(AR.Results.combined, aes(x=Assesment,y=Change.Rate,colour=Assesment))+
                geom_boxplot()+
                ylab("Change with respect to Baseline [%]")+xlab("Assessment Report")+
                scale_color_discrete(name="Assesstment\nReport")+
                theme(legend.position="bottom")


        #FIGURE 4 - Compilation
        chr <- function(n) { rawToChar(as.raw(n)) }

        #Quantile Table
        Q.table.q<-signif(quantile(AR.Results2$Change.Rate,c(0,0.1,0.25,0.5,0.75,0.9,1)),2)

        Q.table.m<-signif(mean(AR.Results2$Change.Rate),2)

        Q.table.count<-round(length(AR.Results2$Change.Rate),0)

        Q.table<-rbind(as.data.frame(Q.table.q),mean=Q.table.m,count=Q.table.count)


        row.names(Q.table)<-c("Min CM","10% CM","25% CM","50% CM","75% CM","90% CM","Max CM","Mean CM",
                              paste("N",chr(176)," Experiment",sep="") )
        names(Q.table)="ALL"

        #Ranges for table
        x.range<-max(AR.Results2$Change.Rate)-min(AR.Results2$Change.Rate)
        x.min<-min(AR.Results2$Change.Rate)+x.range*0.8
        x.max<-min(AR.Results2$Change.Rate)+x.range*0.95

        Used.regressions<-Reanalysis.figure$Regression[Reanalysis.figure$Regression$Significance>=0.95,]
        Used.MK.results<-Reanalysis.figure$MK.results[Reanalysis.figure$Regression$Significance>=0.95,]

        #Regression lines
        Fig4.df<-data.frame(Regression=Used.MK.results$Method,
                            x=Used.MK.results[,4],
                            xend=Used.MK.results[,4],
                            y=rep(0,times = nrow(Used.regressions)),
                            yend=rep(1,times = nrow(Used.regressions)),stringsAsFactors = FALSE)

        #Regression points
        Fig4.point.df<-data.frame(Regression=rep(Used.MK.results$Method,
                                                 each=6),
                                  x=rep(Used.MK.results[,4],
                                        each=6),
                                  y=rep(seq(0,1,length.out=6),
                                        times = nrow(Used.MK.results)))


        #Design Criteria!

        CC.ecdf<-ecdf(x = AR.Results2$Change.Rate)

                #use just estimations over 95% statistical confidence
        if(sum(Reanalysis.figure$Regression$Significance>=0.95,na.rm = T)==0){

                Reanalysis.proy<-min(AR.Results2$Change.Rate)
                suggested.Yvalue<-0.5
        }else{
                Reanalysis.proy<-mean(Reanalysis.figure$MK.results$Evaluation[Reanalysis.figure$Regression$Significance>=0.95])

                suggested.Yvalue<-max(CC.ecdf(Reanalysis.proy),0.5)

        }

        #Adjust the ecdf to a smooth curve
        ecdf.fc.mm<-seq(from = min(AR.Results2$Change.Rate),to = max(AR.Results2$Change.Rate),length.out = 200)

        ecdf.df<-data.frame(x=ecdf.fc.mm,y=CC.ecdf(ecdf.fc.mm))

        model<-loess(ecdf.df$y~ecdf.df$x,span=0.25)

        #plot(predict(model))

        ecdf.df<-data.frame(x=ecdf.fc.mm,y=CC.ecdf(ecdf.fc.mm),y2=predict(model,ecdf.df$x))



        #model for the adjusted curve
        CC.opt.ecdf<-function(x){
                (predict(model,x)-suggested.Yvalue)^2
        }


        Opt.Results<-optimize(f=CC.opt.ecdf,
                              interval=c(min(AR.Results2$Change.Rate),max(AR.Results2$Change.Rate)),
                              #lower=-100,
                              lower=min(AR.Results2$Change.Rate),
                              upper=max(AR.Results2$Change.Rate),
                              maximum=F,
                              tol=1e-10)
        #Adjust the Suggested.Xvalue based on SuggestedY.Value
        if(suggested.Yvalue==1){
                suggested.Xvalue<-max(AR.Results2$Change.Rate)

        }else{
                suggested.Xvalue<-Opt.Results$minimum
        }




        #         CC.ecdf(Opt.Results$par)-suggested.Yvalue

        if(units1=="degC"){
                units2<-"K"
        }else{
                units2<-Reanalysis.figure$units
        }

        suggested.df<-data.frame(x=suggested.Xvalue,
                                 y=suggested.Yvalue,
                                 text=paste("Recommended\nValue: ",signif(suggested.Xvalue,2),"% [",units2,"]",sep=""))

        if(nrow(Fig4.df)>0){


                Fig4<-ggplot(data=AR.Results.combined)+
                        #Cumulative probability
                        stat_ecdf(data=AR.Results2, aes(x=Change.Rate,color="ALL"),size=1)+
                        scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1),
                                           labels=c("0","10","20","30","40","50","60","70","80","90","100"))+
                        ylab("Cumulative Probability (CM) [%]")+xlab("Change with respect to Baseline [%]")+
                        scale_color_discrete(name="Assessment\n Report")+

                        geom_path(data=ecdf.df,aes(x=x,y=y2,color="ALL"))    +
                        #Table
#                         annotation_custom(tableGrob(Q.table,show.namesep =TRUE, gp=gpar(cex=0.8, lwd=2)),
#                                           xmin=x.min,xmax=x.max,ymin=0.02,ymax=0.6)+
                        #Regression line

                        geom_segment(data=Fig4.df, aes(x=x,y=y, xend=xend, yend=yend,linetype=Regression),size=1,
                                     color="black")+
                        #Points
                        geom_point(data=Fig4.point.df, aes(x=x,y=y,shape=Regression),size=4,fill="white")+
                        scale_shape_manual(values=c(0,1,2,5,6), name="Regression")+
                        theme(legend.position="bottom")+
                        #Suggested Value
                        geom_point(data=suggested.df, aes(x=x,y=y),size=6, colour="red")+
                        geom_text(data=suggested.df,aes(x=x,y=y, label=text,fontface=2 ),size=4,hjust=-0.2,vjust=0.6)+
                        guides(shape = guide_legend(ncol = 3))


                Fig4b<-ggplot(data=AR.Results.combined)+
                        #Cumulative probability
                        stat_ecdf(data=AR.Results2, aes(x=Change.Rate,color="ALL"),size=1)+
                        scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1),
                                           labels=c("0","10","20","30","40","50","60","70","80","90","100"))+
                        ylab("Cumulative Probability (CM) [%]")+xlab("Change with respect to Baseline [%]")+
                        scale_color_discrete(name="Assessment\n Report")+

                        geom_path(data=ecdf.df,aes(x=x,y=y2,color="ALL"))+
                        guides(shape = guide_legend(ncol = 3))
                        #Table
                        #                         annotation_custom(tableGrob(Q.table,show.namesep =TRUE, gp=gpar(cex=0.8, lwd=2)),
                        #                                           xmin=x.min,xmax=x.max,ymin=0.02,ymax=0.6)+





        } else{
                Fig4<-ggplot(data=AR.Results.combined)+
                        #Cumulative probability
                        stat_ecdf(data=AR.Results2, aes(x=Change.Rate,color="ALL"),size=1)+
                        scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1),
                                           labels=c("0","10","20","30","40","50","60","70","80","90","100"))+
                        ylab("Cumulative Probability (CM) [%]")+xlab("Change with respect to Baseline [%]")+
                        scale_color_discrete(name="Assessment\n Report")+

                        geom_path(data=ecdf.df,aes(x=x,y=y2,color="ALL"))    +
                        #Table
#                         annotation_custom(tableGrob(Q.table,show.namesep =TRUE, gp=gpar(cex=0.8, lwd=2)),
#                                           xmin=x.min,xmax=x.max,ymin=0.02,ymax=0.6)+
                        #Regression line

#                         geom_segment(data=Fig4.df, aes(x=x,y=y, xend=xend, yend=yend,linetype=Regression),size=1,
#                                      color="black")+
                        #Points
#                         geom_point(data=Fig4.point.df, aes(x=x,y=y,shape=Regression),size=4,fill="white")+
#                         scale_shape_manual(values=c(0,1,2,5,6), name="Regression")+
                        theme(legend.position="bottom")+
                        #Suggested Value
                        geom_point(data=suggested.df, aes(x=x,y=y),size=6, colour="red")+
                        geom_text(data=suggested.df,aes(x=x,y=y, label=text,fontface=2 ),size=4,hjust=-0.2,vjust=0.2)+
                        guides(shape = guide_legend(ncol = 3))

                Fig4b<-ggplot(data=AR.Results.combined)+
                        #Cumulative probability
                        stat_ecdf(data=AR.Results2, aes(x=Change.Rate,color="ALL"),size=1)+
                        scale_y_continuous(breaks=seq(0,1,0.1), limits=c(0,1),
                                           labels=c("0","10","20","30","40","50","60","70","80","90","100"))+
                        ylab("Cumulative Probability (CM) [%]")+xlab("Change with respect to Baseline [%]")+
                        scale_color_discrete(name="Assessment\n Report")+

                        geom_path(data=ecdf.df,aes(x=x,y=y2,color="ALL"))+
                        guides(shape = guide_legend(ncol = 3))


        }



        #Preparing GLOBAL FIGURE

        Fig1<<-Fig1
        Fig2<<-Fig2
        Fig4<<-Fig4
        Fig4b<<-Fig4b

        #Presenting Figures
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(2,10)))
        vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
        suppressWarnings(print(Fig1, vp = vplayout(1, 1:10)))  # key is to define vplayout
        print(Fig2, vp = vplayout(2, 1:2))
        print(Fig3, vp = vplayout(2, 3:6))
        print(Fig4, vp = vplayout(2, 7:10))


        if(pdf.out==TRUE){
                Parameter.text<-sub(">","",Parameter.text)
                Parameter.text<-sub("/",".",Parameter.text)
                filename<-paste(title.place," ",Parameter.text," ",Baseline.start,"-",Baseline.end,"x",Analysis.start,"-",Analysis.end,".pdf",sep="")
                pdf(file=filename, width=20,height=11,pointsize=8)
                #Presenting Figures
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(2,10)))
                vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
                suppressWarnings(print(Fig1, vp = vplayout(1, 1:10)))  # key is to define vplayout
                print(Fig2, vp = vplayout(2, 1:2))
                print(Fig3, vp = vplayout(2, 3:6))
                print(Fig4, vp = vplayout(2, 7:10))

                dev.off()


        }

        suggested.df

}


# AR1_4.get.AR5.all<-function(){
#         library("RCurl", lib.loc="~/R/win-library/3.2")
#         for(i in 1:nrow(AR5.Index)){
#
#                 URL.filename.t<-paste("tas_Amon_",
#                                       AR5.Index$ID.1[i],"_",
#                                       AR5.Index$Scenario[i],
#                                       AR5.Index$Run_ID_T[i],"_",
#                                       AR5.Index$Date[i],
#                                       ".nc",
#                                       sep="")
#
#                 URL.CMIP5<-paste("www.cccsn.ec.gc.ca/files/cmip5/tas/",URL.filename.t,sep="")
#
#                 print(paste(i,url.exists(URL.CMIP5),URL.filename.t))
#
#                 Temp<-download.file(url=URL.CMIP5,destfile = URL.filename.t,method="libcurl",mode="wb")
#
#                 URL.filename.p<-paste("pr_Amon_",
#                                       AR5.Index$ID.1[i],"_",
#                                       AR5.Index$Scenario[i],
#                                       AR5.Index$Run_ID_P[i],"_",
#                                       AR5.Index$Date[i],
#                                       ".nc",
#                                       sep="")
#
#
#                 URL.CMIP5<-paste("www.cccsn.ec.gc.ca/files/cmip5/pr/",URL.filename.p,sep="")
#
#                 print(paste(i,url.exists(URL.CMIP5),URL.filename.p))
#
#                 Pp<-download.file(url=URL.CMIP5,destfile = URL.filename.p,method="libcurl")
#         }
# }


# CMIP5 Analysis - netcf files --------------------------------------------

#'Open one monthly NCDF file and report the output based on lat, long, var and start and end date.
#'@export
nc.open.monthly<-function(path=NULL,filename, var, Latitude, Longitude, start=-1, end=-1){

        #Libraries
        library("ncdf.tools")
        library(ncdf4)
        library("zoo")
        library("lubridate")
        library("signal")
        library("stringr")

        #use path
        path<-if(is.null(path)){getwd()}else{path}
        filename<-paste(path,"/",filename,sep="")

        #Open ncdf file
        ncfile<-nc_open(filename)

        #Latitude definition
        LatIdx<-which(abs(ncfile$dim$lat$vals-Latitude)==
                              min(abs(ncfile$dim$lat$vals-Latitude)))
        Lat.Out<-ncfile$dim$lat$vals[LatIdx]
        #Longitude definition
        LonIdx<-which(abs(ncfile$dim$lon$vals-(if(Longitude<0){Longitude+360}else{Longitude}))==
                              min(abs(ncfile$dim$lon$vals-(if(Longitude<0){Longitude+360}else{Longitude}))))
        Lon.Out<-ncfile$dim$lon$vals[LonIdx]

        #Variable extraction
        nc.var <- ncvar_get(ncfile,varid = var,start=c(LonIdx,LatIdx,1),count=c(1,1,-1))

        #Dates extraction
        nc.dates.raw<-convertDateNcdf2R(ncfile$dim$time$vals,units=word(ncfile$dim$time$units),
                                        origin = strptime(word(ncfile$dim$time$units,start = 3,end = 4),
                                                          format = "%Y-%m-%d %H:%M:%S",tz = "UTC"))

        nc.dates.raw<-as.Date(nc.dates.raw)

        nc.units<-if(var=="pr"){
                nc.units<-"mm"
        } else{
                nc.units<-ncfile$var[[1]]$units
        }

        #Close the nc file
        nc_close(ncfile)

        #Info: if the info is precipitation is transformed in mm/month
        nc.var<-if(var=="pr"){
                #if the variable is precipitation, from km m-2 s-1 to mm/month, 30 is the last month which is assumed to be 30.

                nc.var*c(diff(nc.dates.raw),30)*86400

        } else {
                #if the variable is temperature, the variable is leaved.
                nc.var
        }

        #Correct by the closest first day of the month
        nc.dates.corr<-seq(from=round_date(nc.dates.raw[1],"month"),
                           to=round_date(nc.dates.raw[length(nc.dates.raw)],"month"),
                           by="month" )

        #Adjusting the values based on the adjusting time line and the closest value of the time serie
        nc.var.corr<-interp1(x=as.numeric(nc.dates.raw),
                             y = nc.var,xi=nc.dates.corr,
                             method = "nearest",extrap=T)

        zoo.temp1<-zoo(nc.var,nc.dates.corr)

        #Dummy zoo with complete year of info. Zoo completed by NAs
        nc.dates.corr2<-seq(floor_date(nc.dates.raw[1],"year"),
                            to = floor_date(ceiling_date(nc.dates.raw[length(nc.dates.raw)],"year")-1,"month"),
                            by="month" )
        zoo.temp2<-zoo(NA,nc.dates.corr2)

        #Zoo time series correction fill year gaps with NA
        nc.zoo<-merge(zoo.temp1,zoo.temp2,all = T)
        nc.zoo<-nc.zoo[,-2]
        names(nc.zoo)<-c("var")

        #Original raw information zoo, correction for unique dates
        nc.zoo.raw<-zoo(interp1(x=as.numeric(nc.dates.raw),
                                y=nc.var,
                                xi=as.numeric(unique(nc.dates.raw)),
                                method = "nearest",extrap=NA),
                        unique(nc.dates.raw))

        #Crop the zoo based on start and end dates
        start<-if(start==-1){
                year(nc.zoo[1])
        }else{
                start}
        end<-if(end==-1){
                year(nc.zoo[length(nc.zoo)])
        }else{
                end}

        nc.zoo<-nc.zoo[year(nc.zoo)>=start&year(nc.zoo)<=end,1]

        #Monthly Table
        nc.monthly.table<-zoo2matrix(nc.zoo)

        #Monthly Synthesis
        nc.monthly.syn<-colMeans(nc.monthly.table,na.rm = T)

        #Annual Synthesis
        if(var=="pr"){
                #If the variable is the precipitation, the annual is the sum
                nc.annual.syn<-sum(nc.monthly.syn)
        }else{
                #If the variable is temperature, the annual is the weighted average
                nc.annual.syn<-sum(nc.monthly.syn*c(31,28.25,31,30,31,30,31,31,30,31,30,31))/365.25
        }

        #Output table
        output<-list(nc.zoo.raw,nc.zoo,nc.monthly.table,nc.monthly.syn,nc.annual.syn,Lat.Out,Lon.Out,start,end,nc.units)
        names(output)<-c("raw.timeseries","cor.timeseries","monthly.table","monthly","annual","latitude","longitude","start","end","units")
        output
}

#'Open one daily NCDF file and report the output based on lat, long, var and start and end date.
#'@export
nc.open.daily<-function(path=NULL,filename, FUN=mean, var, Latitude, Longitude, Level=1,start=-1, end=-1){

        #Libraries
        library("ncdf.tools")
        #library("ncdf")
        library(ncdf4)
        library("zoo")
        library("lubridate")
        library("signal")
        library("stringr")
        library("udunits2")
        library("xts")
        library("hydroTSM")

        #use path
        path<-if(is.null(path)){getwd()}else{path}
        filename<-paste(path,"/",filename,sep="")

        #Open ncdf file
        #ncfile<-open.ncdf(con = filename)
        ncfile<-nc_open(filename)

        #Latitude definition
        LatIdx<-which(abs(ncfile$dim$lat$vals-Latitude)==
                              min(abs(ncfile$dim$lat$vals-Latitude)))
        Lat.Out<-ncfile$dim$lat$vals[LatIdx]
        #Longitude definition
        LonIdx<-which(abs(ncfile$dim$lon$vals-(if(Longitude<0){Longitude+360}else{Longitude}))==
                              min(abs(ncfile$dim$lon$vals-(if(Longitude<0){Longitude+360}else{Longitude}))))
        Lon.Out<-ncfile$dim$lon$vals[LonIdx]


        #Dates extraction
        nc.valid<-ncfile$dim$time$vals>0

        #Variable extraction
        if(ncfile$ndims==3){
                nc.var <- ncvar_get(ncfile,varid = var,start=c(LonIdx,LatIdx,1),count=c(1,1,-1))
        } else if(ncfile$ndims==4){
                message(paste("Levels [",ncfile$dim$level$units,"] :",sep=""))
                print(ncfile$dim$level$vals)
                message(paste("Selected :",ncfile$dim$level$vals[Level]))
                nc.var <- ncvar_get(ncfile,varid = var,start=c(LonIdx,LatIdx,Level,1),count=c(1,1,1,-1))
        }else{
                print("nc.open.daily not prepared for more than 5 dimensions")
        }



        nc.var<-nc.var[nc.valid]

        nc.dates.raw<-convertDateNcdf2R(ncfile$dim$time$vals,units=word(ncfile$dim$time$units),
                                        origin = strptime(word(ncfile$dim$time$units,start = 3,end = 4),
                                                          format = "%Y-%m-%d %H:%M:%S",tz = "UTC"))

        nc.dates.raw<-nc.dates.raw[nc.valid]



        nc.units<-if(var=="pr"){
                nc.units<-"mm"
        } else{
                nc.units<-ncfile$var[[1]]$units
        }

        #Close the nc file
        nc_close(ncfile)

        #Info: if the info is precipitation is transformed in mm/month
        nc.var<-if(var=="pr"){
                #if the variable is precipitation, from km m-2 s-1 to mm/month, 30 is the last month which is assumed to be 30.

                nc.var*c(diff(nc.dates.raw),30)*86400

        } else {
                #if the variable is temperature, the variable is leaved.
                nc.var
        }
        #SUBDAILY PREPARATION

        if(ud.convert(x = as.numeric(diff(nc.dates.raw)[1]),u1 =  word(ncfile$dim$time$units),u2 = "day")<1){

                nc.zoo.subdaily<-zoo(nc.var, nc.dates.raw)
                subdaily<-TRUE

        }else{
                subdaily<-FALSE
                nc.dates.raw<-as.Date(nc.dates.raw)
        }



        #DAILY PREPARATION

        if (subdaily==TRUE){

                nc.zoo.daily<-apply.daily(nc.zoo.subdaily,FUN)

                nc.zoo.daily<-zoo(coredata(nc.zoo.daily),as.Date(time(nc.zoo.daily)))

        } else{

                #Remove zero values in time
                nc.zoo.daily<-zoo(nc.var[nc.dates.raw>=0], nc.dates.raw[nc.dates.raw>=0])

        }

        #Create a perfect time series
        nc.zoo.extend<-zoo(NA,seq(from = as.Date(paste(year(start(nc.zoo.daily)),"-01-01",sep=""),format = "%Y-%m-%d"),
                                  to=as.Date(paste(year(end(nc.zoo.daily)),"-12-31",sep=""),format = "%Y-%m-%d"),
                                  by= "day"))

        nc.zoo.daily.c<-merge(nc.zoo.daily,nc.zoo.extend,all = T)

        #leave time series with NA
        nc.zoo.daily<-nc.zoo.daily.c[,-2]

        #MONTHLY PREPARATION

        #monthly preparation
        nc.zoo.monthly<-daily2monthly(nc.zoo.daily,FUN,na.rm=TRUE)

        avail.days<-as.vector(as.matrix(t(dwi(nc.zoo.daily,out.unit = "mpy"))))

        max.days<-as.numeric(days_in_month(seq(from = as.Date(paste(first(year(nc.zoo.daily)),"-01-01",sep=""),format = "%Y-%m-%d"),
                                               to=as.Date(paste(last(year(nc.zoo.daily)),"-12-31",sep=""),format = "%Y-%m-%d"),
                                               by= "month")))

        #months with more than 5 days missing
        nc.zoo.monthly<-nc.zoo.monthly[max.days-avail.days<=5]

        #Create a perfect time series
        nc.zoo.extend<-zoo(NA,seq(from = as.Date(paste(year(start(nc.zoo.daily)),"-01-01",sep=""),format = "%Y-%m-%d"),
                                  to=as.Date(paste(year(end(nc.zoo.daily)),"-12-31",sep=""),format = "%Y-%m-%d"),
                                  by= "month"))

        nc.zoo.monthly.c<-merge(nc.zoo.monthly,nc.zoo.extend,all = T)

        #leave time series with NA
        nc.zoo.monthly<-nc.zoo.monthly.c[,-2]


        #YEARLY PREPARATION

        #Yearly preparation. Years with more than 360 days
        nc.zoo.yearly<-daily2annual(nc.zoo.daily,FUN,na.rm=TRUE)[as.logical(dwi(nc.zoo.daily,out.unit = "years")>=360)]

        nc.zoo.yearly<-zoo(coredata(nc.zoo.yearly),year(time(nc.zoo.yearly)))

        #MONTHLY TABLE PREPARATION
        #Crop the zoo based on start and end dates
        start<-if(start==-1){
                year(nc.zoo.monthly[1])
        }else{
                start}
        end<-if(end==-1){
                year(nc.zoo.monthly[length(nc.zoo.monthly)])
        }else{
                end}

        nc.zoo<-nc.zoo.monthly[year(nc.zoo.monthly)>=start&year(nc.zoo.monthly)<=end,1]


        #Monthly Table
        nc.monthly.table<-zoo2matrix(nc.zoo)

        #Monthly Synthesis
        nc.monthly.syn<-colMeans(nc.monthly.table,na.rm = T)

        #Annual Synthesis
        if(var=="pr"|var=="tp"){
                #If the variable is the precipitation, the annual is the sum
                nc.annual.syn<-sum(nc.monthly.syn)
        }else{
                #If the variable is temperature, the annual is the weighted average
                nc.annual.syn<-sum(nc.monthly.syn*c(31,28.25,31,30,31,30,31,31,30,31,30,31))/365.25
        }

        #Output table
        if(subdaily==TRUE){

                output<-list(nc.zoo.subdaily,nc.zoo.daily,nc.zoo.monthly,nc.zoo.yearly,nc.monthly.table,nc.monthly.syn,nc.annual.syn,Lat.Out,Lon.Out,start,end,nc.units)
                names(output)<-c("subdaily.timeseries","daily.timeseries","monthly.timeseries","yearly.timeseries","monthly.table","monthly","annual","latitude","longitude","start","end","units")


        }else{


                output<-list(nc.zoo.daily,nc.zoo.monthly,nc.zoo.yearly,nc.monthly.table,nc.monthly.syn,nc.annual.syn,Lat.Out,Lon.Out,start,end,nc.units)
                names(output)<-c("daily.timeseries","monthly.timeseries","yearly.timeseries","monthly.table","monthly","annual","latitude","longitude","start","end","units")
        }


        output
}



#'Report all the results for the netcdf files based on a index file and parameters
#'@export
AR5.get.results<-function(path,Baseline.start, Baseline.end, Analysis.start, Analysis.end, Latitude, Longitude, Parameter){
        #Join Parameter from CC to Variable from CMIP5
        if(Parameter==61){#|#Total Precipitation
                #Parameter==235|#Precipitation - Days > 10mm/d
                #Parameter==237|#Precipitation - 5 Day Max
                #Parameter==238|#Precipitation - Simple Daily Intensity Index
                #Parameter==236) {#Days - Consecutive Dry
                var<-"pr"
        }else{
                if(Parameter==11){
                        var<-"tas"
                }else{
                        var<-NULL
                        return(NULL)}
        }



        #Create a raw results for AR5
        AR5.Raw_Results<-list(NULL,NULL,NULL,NULL)


        #DB is created for AR5
        AR5.Results<-data.frame(Models=unique(AR5.Index$Name),"historical"=NA,rcp26=NA,rcp45=NA,rcp85=NA,stringsAsFactors = F)


        #Complete table AR5.Results

        for (i in 1:nrow(AR5.Results)){
                #  i<-26
                #CMIP5.Idx<-AR5.Index$ID[AR5.Index$Name==AR5.Results$Models[i]&AR5.Index$Variable==var]

                CMIP5.Idx<-which((AR5.Index$Name==AR5.Results$Models[i]&AR5.Index$Variable==var)==TRUE)

                print(paste(i,"/",nrow(AR5.Results)))

                #Historical
                nc.historical<-nc.open.monthly(path=path,
                                               filename = AR5.Index$Filename[CMIP5.Idx[4]],
                                               var = var, Latitude = Latitude, Longitude = Longitude,
                                               start=Baseline.start, end = Baseline.end)

                AR5.Results$historical[i]<-nc.historical$annual



                #output Historical
                print(paste(AR5.Results$Models[i],
                            " - Historical ","(", Baseline.start,"-",Baseline.end,")",sep=""))


                #rcp26
                nc.rcp26<-nc.open.monthly(path=path,
                                          filename = AR5.Index$Filename[CMIP5.Idx[1]],
                                          var = var, Latitude = Latitude, Longitude = Longitude,
                                          start=Analysis.start, end = Analysis.end)

                AR5.Results$rcp26[i]<-nc.rcp26$annual


                #output rcp 2.6
                print(paste(AR5.Results$Models[i],
                            " - rcp 2.6 ","(", Analysis.start,"-",Analysis.end,")",sep=""))


                #rcp4.55
                nc.rcp45<-nc.open.monthly(path=path,
                                          filename = AR5.Index$Filename[CMIP5.Idx[2]],
                                          var = var, Latitude = Latitude, Longitude = Longitude,
                                          start=Analysis.start, end = Analysis.end)

                AR5.Results$rcp45[i]<-nc.rcp45$annual

                #output rcp 4.5
                print(paste(AR5.Results$Models[i],
                            " - rcp 4.5 ","(", Analysis.start,"-",Analysis.end,")",sep=""))

                #rcp85
                nc.rcp85<-nc.open.monthly(path=path,
                                          filename = AR5.Index$Filename[CMIP5.Idx[3]],
                                          var = var, Latitude = Latitude, Longitude = Longitude,
                                          start=Analysis.start, end = Analysis.end)

                AR5.Results$rcp85[i]<-nc.rcp85$annual

                #output rcp 8.5
                print(paste(AR5.Results$Models[i],
                            " - rcp 8.5 ","(", Analysis.start,"-",Analysis.end,")",sep=""))


                #save information
                temp<-list(nc.historical, nc.rcp26, nc.rcp45, nc.rcp85)
                names(temp)<-c("historical","rcp26","rcp45","rcp85")

                AR5.Raw_Results[i]<-list(temp)

        }


        #Saving output from AR5
        names(AR5.Raw_Results)<-make.names(unique(AR5.Index$Name))

        AR5.Raw_Results<<-AR5.Raw_Results

        save(AR5.Raw_Results,file="AR5.Raw_Results.RData",ascii=FALSE)

        #Reorganize the table based on previous dataframe for AR1_4

        AR5.Results.Temp<-data.frame(ID=1101:(1100+3*nrow(AR5.Results)),
                                     Assesment="AR5",
                                     Experiment=do.call(paste, expand.grid("AR 5",AR5.Results$Models,c("- RCP 2.6", "- RCP 4.5", "- RCP 8.5"))),
                                     Baseline.Mean=rep(AR5.Results$historical,times = 3),
                                     Analysis.Mean=c(AR5.Results$rcp26,AR5.Results$rcp45,AR5.Results$rcp85),
                                     Change.Rate=NA)
        AR5.Results.Temp$Change.Rate<-100*(AR5.Results.Temp$Analysis.Mean-AR5.Results.Temp$Baseline.Mean)/AR5.Results.Temp$Baseline.Mean

        #Temperature transformation from Kelvin to Celsius
        if(var=="tas"){
                AR5.Results.Temp[,4:5]<-AR5.Results.Temp[,4:5]-273.15
        }

        #Results presentation
        if(exists("AR1_4.Results")){
                AR1_5.Results<<-rbind(AR1_4.Results,AR5.Results.Temp)
        }

        AR5.Results<<-AR5.Results.Temp

}

#'Transform a zoo time series in a table
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


#' Index table for AR 1 to AR 4
#' @author Victor Munoz
#' @references EC
#' @format Index table for AR1 to AR 4
#' @name AR1_4.Index
#' @usage data(AR1_4.Index)
NULL

#' Index table for AR 5
#' @author Victor Munoz
#' @references EC
#' @format Index table for AR1 to AR 4
#' @name AR5.Index
#' @usage data(AR5.Index)
NULL

