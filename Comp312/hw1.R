#=====================
#These first few lines are just creating a (somewhat) more neat data frame.
#Most of its responsabilities were absorbed by the Department of Health when Mayor Rahm was elected.
#I wanted to look at the nature and locations of each of the inspections, to see if this was an appropriate measure or not.
#Specifically, I'm looking at the addresses and 'narratives' of all the inspections of the short lived Department of Environment of Chicago.
#So in the future, I'll probably want to compare these data frames with similar ones from the Department of Health post merger.
#1) Read CSV
#2) Split CSV into two df: Historical DoE & the Dep. Public Health
#3) (for e/ of the two df) alter dates from unusable combined factor level to seperated columns: month, day, year, and date (not factor)
#4) (for e/ of the two df) Fill in missing addresses with the given information, and filing out N/A to the rest
#5) Further analysis
#=====================
#++++Initializing++++
library(reshape2)
library(ggplot2)
library(plyr)
setwd("~/Documents/Comp312/hw1/")

OriginalArray <- read.csv("~/Documents/Comp312/hw1/CDPH_Environmental_Inspections.csv", header=TRUE, stringsAsFactors=FALSE) #This will take a while.
print("Done loading csv")
#=====================
#++++Seperating DoE & DPH++++
#2
DoEInspections <- OriginalArray[OriginalArray$DATA.SOURCE == "HISTORIC DEPT. OF ENVIRONMENT", ]
PostDoE<- OriginalArray[OriginalArray$DATA.SOURCE == "DEPT. OF PUBLIC HEALTH", ]
#3
DoEInspections$Date<-as.Date(DoEInspections$INSPECTION.DATE, format = "%m/%d/%Y")
PostDoE$Date<-as.Date(PostDoE$INSPECTION.DATE, format = "%m/%d/%Y")
#Oh boy here we go
DoEHits<- data.frame(Date=as.Date(character()), #Initializing empty array 
                     Hits=integer(), 
                     stringsAsFactors=FALSE) 
PDoEHits<- data.frame(Date=as.Date(character()), #Initializing second empty array 
                      Hits=integer(), 
                      stringsAsFactors=FALSE) 

DoEHits<-as.data.frame(table(DoEInspections$Date)) #Need to stop time travelers; some date's are recorded as year '10' and year '3000'.  
PDoEHits<-as.data.frame(table(PostDoE$Date))

{#Stopping time travelers:
  DoEHits <- DoEHits[-c(1,2,3,4,5,5433),]
  DoEHits$Freq[DoEHits$Var1 == "2001-09-10"] <- DoEHits$Freq[DoEHits$Var1 == "2001-09-10"]+1
  DoEHits$Freq[DoEHits$Var1 == "2003-12-10"] <- DoEHits$Freq[DoEHits$Var1 == "2003-12-10"]+1
  DoEHits$Freq[DoEHits$Var1 == "2006-02-04"] <- DoEHits$Freq[DoEHits$Var1 == "2006-02-04"]+1
  DoEHits$Freq[DoEHits$Var1 == "2005-03-30"] <- DoEHits$Freq[DoEHits$Var1 == "2005-03-30"]+1
}
#=====================
#++++Checking for overlapping dates++++
#Just testing out incrementing and adding values in R
#Here, i wanted to see what (if any) dates overlap between the old DoE & the DPH of Chicago
test<- DoEHits$Var1 %in% PDoEHits$Var1 #This prints out all the variances that are true/false.  
#However, since I am not a robot (yet) I cannot pick out from the 5400+ variances of FALSE to see which few are TRUE.
#So this loop SHOULD help me see which few are true.  This is mostly a practice, when (if) I complete it, i'll cut it out of this script and create a new one to store it in.
test2<- 0
i<-1
while (isTRUE(i < 5428)) {
  if (isTRUE(test[i])) {
    print(test[i])
    i <- i + 1
  } else {
    i <- i + 1
  }
}
#=====================
#++++Creating a new df (w/ sequential dates) & graphing++++
TotalHits <- data.frame (Date=as.Date(character()),
                       DoEHits=integer(),
                       PDoEHits=integer(),
                       stringsAsFactors=FALSE)

i<-1
j<-1
iDate=as.Date("1954-10-19")

while (isTRUE(iDate <= as.Date("2017-06-06"))) {
  if (isTRUE(iDate == as.Date(DoEHits$Var1[i]))) {#if the dates in TotalHits & table match, add the freq from TotalHits & table and put it in TotalHits$Hits
    foo <- list(Date = as.Date(iDate), #how to make a mixed value array in R POST NOTE THANK YOU /dpt/
                DoEHits = as.numeric(DoEHits$Freq[i]), 
                PDoEHits = as.numeric(0)) 
    bar <- as.data.frame(lapply(foo, unlist))
    print("HIT")
    print(iDate)
    TotalHits<-rbind(TotalHits, bar)
    iDate <- iDate + 1
    i <- i + 1 #Increment i (next col of DoEHits)
  } else {
    foo <- list (Date = as.Date(iDate),
                 DoEHits = as.numeric(0),
                 PDoEHits = as.numeric(0))
    bar <- as.data.frame(lapply(foo, unlist))
    print("NO HIT")
    print(iDate)
    TotalHits<-rbind(TotalHits, bar)
    iDate <- iDate + 1
  }
}

i<-1
iDate<-as.Date("2012-01-03")
while (isTRUE(iDate <= as.Date("2017-06-06"))) { #Same as other while loop but or DPH
  if (isTRUE(iDate == as.Date(PDoEHits$Var1[i]))) {
    foo <- list(Date = as.Date(iDate), 
                DoEHits = 0, #Nominally, I should make this equal to what TotalHits$DoEHits already is, but since we know there is no overlap between the two it is safe to say they are all 0
                PDoEHits = as.numeric(PDoEHits$Freq[i])) 
    bar <- as.data.frame(lapply(foo, unlist))
    print("HIT")
    print(iDate)
    TotalHits<-rbind(TotalHits, bar)
    iDate <- iDate + 1
    i <- i + 1
  } else {
    foo <- list (Date = as.Date(iDate),
                 DoEHits = as.numeric(0),
                 PDoEHits = as.numeric(0))
    bar <- as.data.frame(lapply(foo, unlist))
    print("NO HIT")
    print(iDate)
    TotalHits<-rbind(TotalHits, bar)
    iDate <- iDate + 1
  }
}
#=====================
#Plotting the data
barplot(`colnames<-`(t(TotalHits[-1]), TotalHits[,1]), beside=TRUE, 
        legend.text = TRUE, col = c("red", "green"), 
        args.legend = list(x = "topleft", bty = "n", inset=c(-0.05, 0)))
#=====================
#Everything below here is not really being looked at vOv
#This is more for mapping, but more on that later...
i<-1
for (i in 1:dim(DoEInspections)) {
  if (DoEInspections$MAPPED.LOCATION[i] == "") {
    DoEInspections$MAPPED.LOCATION[i] = paste(DoEInspections$STREET.NUMBER.FROM[i],DoEInspections$STREET.NAME[i],DoEInspections$STREET.TYPE[i])
  }
}
DoEInspections$MAPPED.LOCATION[DoEInspections$MAPPED.LOCATION == ""] <- NA

PostDoE$MAPPED.LOCATION[PostDoE$MAPPED.LOCATION == ""] <- NA
