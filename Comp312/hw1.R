#=====================
#PSEUDOCODE
#1) Read CSV
#2) Split CSV into two df: Historical DoE & the Dep. Public Health
#3) (for e/ of the two df) alter dates from unusable combined factor level to seperated columns: month, day, year, and date (not factor)
#4) Putting both Hits into one df by number of hits, and categorizing them
#5) Final Graph
#=====================
#1) Read CSV
library(reshape2)
library(ggplot2)
library(plyr)
setwd("~/Documents/Comp312/hw1/")

OriginalArray <- read.csv("~/Documents/Comp312/hw1/CDPH_Environmental_Inspections.csv", header=TRUE, stringsAsFactors=FALSE) #This will take a while.
print("Done loading csv")
#=====================
#2) Split CSV into two df: Historical DoE & the Dep. Public Health
DoEInspections <- OriginalArray[OriginalArray$DATA.SOURCE == "HISTORIC DEPT. OF ENVIRONMENT", ]
PostDoE<- OriginalArray[OriginalArray$DATA.SOURCE == "DEPT. OF PUBLIC HEALTH", ]
DoEInspections$Date<-as.Date(DoEInspections$INSPECTION.DATE, format = "%m/%d/%Y")
PostDoE$Date<-as.Date(PostDoE$INSPECTION.DATE, format = "%m/%d/%Y")
#=====================
#3) (for e/ of the two df) alter dates from unusable combined factor level to seperated columns: month, day, year, and date (not factor)
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
#This is not part of the main graph, but just to see if there were any overlap of dates
test<- DoEHits$Var1 %in% PDoEHits$Var1   

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
#4) Putting both Hits into one df by number of hits, and categorizing them
TotalHits <- data.frame (Date=as.Date(character()),
                       Hits=integer(),
                       Catgory = character(),
                       stringsAsFactors=FALSE)

i<-1
iDate=as.Date("1954-10-19")

while (isTRUE(iDate <= as.Date("2017-06-06"))) {
  if (isTRUE(iDate == as.Date(DoEHits$Var1[i]))) {#if the dates in TotalHits & table match, add the freq from TotalHits & table and put it in TotalHits$Hits
    foo <- list(Date = as.Date(iDate), #how to make a mixed value array in R POST NOTE THANK YOU /dpt/
                Hits = as.numeric(DoEHits$Freq[i]), 
                Category = as.character("Department of Environment"))
    bar <- as.data.frame(lapply(foo, unlist), stringsAsFactors = FALSE)
    print("HIT")
    print(iDate)
    TotalHits<-rbind(TotalHits, bar)
    iDate <- iDate + 1
    i <- i + 1 #Increment i (next col of DoEHits)
  } else {
    foo <- list (Date = as.Date(iDate),
                 Hits = as.numeric(0),
                 Category = "NA")
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
    print("HIT")
    print(iDate)
    TotalHits$Hits[TotalHits$Date == iDate] <- PDoEHits$Freq[i]
    TotalHits$Category[TotalHits$Date == iDate] <- "Department of Public Health"
    iDate <- iDate + 1
    i <- i + 1
  } else {
    print("NO HIT")
    print(iDate)
    iDate <- iDate + 1
  }
}
#=====================
#5) Final Graph
ggplot(TotalHits, aes(x=Date, y = Hits, group = Category, fill = Category)) + geom_area()