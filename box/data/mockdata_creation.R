

mock <- read.csv("C:/Users/John/Documents/GitHub/grad_data/box/data/subset_data.csv")

library(dplyr)
library(tidyr)

newvar.list <- lapply(levels(as.factor(mock$HegisDesc)), function(x){
  l1 <- paste("Hegis", x, sep="");
  l1 <- mock%>%
    filter(HegisDesc %in%c(x))%>%
    select(TTD);
  old.names <- names(l1)
  l1[,(ncol(l1)+1)] <- 1:length(l1[,1]);
  names(l1) <- c(paste(old.names, x, sep=""), "MyRowName");
  l1
})





joinFunc <- function(DFList, NewCounter = 2){
  temp.data <- full_join(DFList[[NewCounter-1]], DFList[[NewCounter]], by = "MyRowName");
  newData <<- temp.data;
  recursiveJoin(DFList = DFList, NewCounter = 3, newData2 = newData)
}

recursiveJoin <- function(DFList, NewCounter, newData2){
  newData2 <- full_join(newData2, DFList[[NewCounter]], by = "MyRowName");
  if(NewCounter == length(DFList)){
    print("Done");
    select(newData2, -MyRowName);
  }else{
    recursiveJoin(DFList = DFList, NewCounter = (NewCounter+1), newData2 = newData2)
    }
}


data.out <- joinFunc(newvar.list)



write.csv(data.out,"C:/Users/John/Documents/GitHub/grad_data/box/data/data_for_Axis.csv", row.names = FALSE, na = "")

