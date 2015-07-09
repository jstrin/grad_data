library(XLConnect)
library(dplyr)

#load workbook
ttdWB <- loadWorkbook("C://Users/jstring/Documents/GitHub/grad_data/box/data/ttd_4-22-2015.xlsx")

# choose measure (form which enrollment date)

measure <- "Graduate.Student..In.Degree.Program."

#load worksheet as dataframe
ttd <- readWorksheet(ttdWB, sheet = 1, startRow = 3)

#get order of measures
TTDorder <- readWorksheet(ttdWB, sheet = 1, startRow = 2, endRow = 2)


#determine what variable set to use based on measure selected above
if(names(TTDorder)[1] == measure){
  varNames <- c("Hegis.Desc", "Grad.YrSem", "Beg.YrSem", "Grd.Doc", "Col", "Deg.Prog.Cd.1",  
                "Reporting.Hegis.Cd.1" ,"Time.To.Degree..Yrs.")
}

if(names(TTDorder)[10] == measure){
  varNames <- c("Hegis.Desc", "Grad.YrSem", "Beg.YrSem.1" , "Grd.Doc.1", "Col.1", "Deg.Prog.Cd.2", 
                "Reporting.Hegis.Cd.2",  "Time.To.Degree..Yrs..1")
}

if(names(TTDorder)[19] == measure){
  varNames <- c("Hegis.Desc", "Grad.YrSem", "Beg.YrSem.2" , "Grd.Doc.3", "Col.2", "Deg.Prog.Cd.3", 
                "Reporting.Hegis.Cd.3",  "Time.To.Degree..Yrs..2")
}

if(names(TTDorder)[28] == measure){
  varNames <- c("Hegis.Desc", "Grad.YrSem", "Beg.YrSem.3" , "Grd.Doc.4", "Col.3", "Deg.Prog.Cd.4", 
                "Reporting.Hegis.Cd.4", "Time.To.Degree..Yrs..3")
}

#subset data
ttdWorking <- ttd[, names(ttd)%in%varNames]

#rename the variables
names(ttdWorking) <- c("Hegis.Desc", "Grad.YrSem", "Beg.YrSem", "Gr.Doc", "College", "Deg.Cd", "Hegis.Cd", "TTD")

# create list of comparison groups
sslist<-list(1515,1914,2001,2202,2203,2204,2205,2206,2207,2208,2220,2253,2255,2314,4961,5507)
mathcslist<-list(1701,1707,701)
physiclist<-list(1902,1905,1911,1926)
physicplusmathlist<-list(1902,1905,1911,1926,1701,1707,701)
lifescienceslist<-list(425,401,419,432,445,449,499,905)
humanitieslist<-list(1504,1003,1006,1013,1132,1135,1501,1504,1509,1515,2256,2311,4905,4992,871 )

ttdWorking$Hegis.Cd <- as.numeric(ttdWorking$Hegis.Cd)

#create new grouping variable for comparison groups
ttdWorking$group <- NA
ttdWorking$group[ ttdWorking$Hegis.Cd%in%sslist] <- "SocSci"
ttdWorking$group[ ttdWorking$Hegis.Cd%in%physicplusmathlist] <- "PhysicMathCS"
ttdWorking$group[ ttdWorking$Hegis.Cd%in%lifescienceslist] <- "LifeSci"
ttdWorking$group[ ttdWorking$Hegis.Cd%in%humanitieslist] <- "Humanities"

#save csv
write.csv(ttdWorking, file = "C://Users/jstring/Documents/GitHub/grad_data/box/data/boxplot_data.csv")
