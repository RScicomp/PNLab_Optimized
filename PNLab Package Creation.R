rm(list=ls())
install.packages('devtools')
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)
create("PNLab")

#'Get the unqiue values from a data frame
#'@Param Dataframe, row number
#'@keywords Parlist
#'@export
#'@examples
#'getUniqueParlist()

getUniqueParlist<- function(data,rowno){
  return(unique(unlist(data[rowno,])))
}


ncorrectstats <- function(dataframe1, dataframe2){
  pars <- asdparticipants()
  #browser()
  incorrect <- combine(dataframe1,dataframe2,pars[1])
  for(i in 2: length(pars)){
    
    incorrect <- cbind(combine(dataframe1,dataframe2, pars[i]),incorrect)
  }
  return(incorrect)
}
incorrectcounter <- function(dataframe){
  
  pars <- append(typparticipants(),asdparticipants())
  a<-table(as.vector(unlist(dataframe[1,])))/3
  c<-as.data.frame(a)
  rownames(c) <- as.vector(c$Var1)
  c$Var1 <-  NULL
  colnames(c) <- c("No. Incorrect")
  return(c)
}
incorrectstatstyp <- function(dataframe1, dataframe2){
  pars <- typparticipants()
  incorrect <- combine(dataframe1,dataframe2,pars[1])
  for(i in 2: length(pars)){
    incorrect <- cbind(combine(dataframe1,dataframe2, pars[i]),incorrect)
  }
  return(incorrect)
}
correctstats <- function(dataframe1,dataframe2){
  pars <- asdparticipants()
  correct<- combine(dataframe1,dataframe2,pars[1])
  for(i in 2: length(pars)){
    correct <- cbind(combine(dataframe1,dataframe2, pars[i]),correct)
    #browser()
  }
  return(correct)
}
correctstatstyp <- function(dataframe1,dataframe2){
  pars <- typparticipants()
  correct<- combine(dataframe1,dataframe2,pars[1])
  for(i in 2: length(pars)){
    correct <- cbind(combine(dataframe1,dataframe2, pars[i]),correct)
    #browser()
  }
  return(correct)
}
allstats <- function(dataframe1, dataframe2){
  pars <- asdparticipants()
  all <- combine(dataframe1,dataframe2, pars[1])
  for (i in 2: length(pars)){
    all <- cbind(combine(dataframe1,dataframe2,pars[i]),all)
  }
  return(all)
}
allstatstyp <- function(dataframe1, dataframe2){
  pars <- typparticipants()
  all <- combine(dataframe1,dataframe2, pars[1])
  for (i in 2: length(pars)){
    all <- cbind(combine(dataframe1,dataframe2,pars[i]),all)
  }
  return(all)
}
parstats <- function(dataframe, name){
  #browser()
  newdataframe = dataframe[,grep(name, dataframe[1,])]
  return(newdataframe)
}
combine <- function(dataframe1, dataframe2, name){
  # browser()
  newdataframe= cbind(parstats(dataframe1, name), parstats(dataframe2,name))
  return(newdataframe)
}
addparticipant <- function(dataframe, name,AOIno,AOI){
  
  # browser()
  newdataframe<- parstats(dataframe, name)
  if(ncol(newdataframe) == 0){
    countsum <- NA
    countavg <- NA
    countstd <- NA
    timesum <- NA
    timeavg <- NA
    timestd <- NA
  }else{
    countseq <- seq(2,ncol(newdataframe),3)
    timeseq <- seq(3,ncol(newdataframe),3)
    #browser()
    counts <- newdataframe[,countseq]
    times <- newdataframe[,timeseq]
    if(ncol(newdataframe) == 3){
      
      #browser()
      count <- append(unlist(counts[AOIno+2]),unlist(counts[AOIno+3]))
      time <- append(unlist(times[AOIno+2]),unlist(times[AOIno+3]))
    }else{
      count <- append(unlist(counts[AOIno+2,]),unlist(counts[AOIno+3,]))
      time <- append(unlist(times[AOIno+2,]),unlist(times[AOIno+3,]))
    }
    #browser()
    countsum <- sum(as.numeric(count))
    timesum <- sum(as.numeric(time))
    countavg <- mean(as.numeric(count))
    timeavg <- mean(as.numeric(time))
    countstd <- sd(as.numeric(count))
    timestd <- sd(as.numeric(time))
  }
  df <- data.frame(countsum,countavg,countstd,timesum,timeavg,timestd)
  rownames(df) <- name
  colnames(df) <- c(paste(AOI,"Count",sep=""), paste(AOI, "Average", sep = ""), paste(AOI, "STD", sep = ""), paste("TimeSpenton", AOI, sep =""), paste("AvgTimeSpenton", AOI, sep=""), paste("STDTimeSpenton", AOI, sep =""))
  
  return(df)  
}
addparticipantLR <- function(dataframe, name,AOIno,AOI){
  
  #browser()
  newdataframe<- parstats(dataframe, name)
  if(ncol(newdataframe) == 0){
    countsum <- NA
    countavg <- NA
    countstd <- NA
    timesum <- NA
    timeavg <- NA
    timestd <- NA
  }else{
    countseq <- seq(2,ncol(newdataframe),3)
    timeseq <- seq(3,ncol(newdataframe),3)
    #browser()
    counts <- newdataframe[,countseq]
    times <- newdataframe[,timeseq]
    if(ncol(newdataframe) == 3){
      
      count <- unlist(counts[AOIno+2])
      time <- unlist(times[AOIno+2])
    }else{
      count <- unlist(counts[AOIno+2,])
      time <- unlist(times[AOIno+2,])
    }
    #browser()
    countsum <- sum(as.numeric(count))
    timesum <- sum(as.numeric(time))
    countavg <- mean(as.numeric(count))
    timeavg <- mean(as.numeric(time))
    countstd <- sd(as.numeric(count))
    timestd <- sd(as.numeric(time))
  }
  df <- data.frame(countsum,countavg,countstd,timesum,timeavg,timestd)
  rownames(df) <- name
  colnames(df) <- c(paste(AOI,"Count",sep=""), paste(AOI, "Average", sep = ""), paste(AOI, "STD", sep = ""), paste("TimeSpenton", AOI, sep =""), paste("AvgTimeSpenton", AOI, sep=""), paste("STDTimeSpenton", AOI, sep =""))
  
  return(df)  
}

parstatsall <- function(name, condition){
  cbind(addparticipant(condition,name,1,"Eye"),addparticipant(condition, name, 3, "Nose"), addparticipant(condition, name,5,"Mouth"), addparticipant(condition,name,7,"Hair"),addparticipant(condition,name,9,"Jaw"))
}
parstatsallL <- function(name, condition){
  cbind(addparticipantLR(condition,name,1,"LFEye"),addparticipantLR(condition, name, 3, "LFNose"), addparticipantLR(condition, name,5,"LFMouth"), addparticipantLR(condition,name,7,"LFHair"),addparticipantLR(condition,name,9,"LFJaw"))
}
parstatsallR <- function(name, condition){
  cbind(addparticipantLR(condition,name,2,"RFEye"),addparticipantLR(condition, name, 4, "RFNose"), addparticipantLR(condition, name,6,"RFMouth"), addparticipantLR(condition,name,8,"RFHair"),addparticipantLR(condition,name,10,"RFJaw"))
}
allparstatsall <- function(condition){
  #browser()
  pars<-asdparticipants()
  dataframe <- parstatsall(pars[1],condition)
  for (i in 2: length(pars)){
    dataframe <- rbind(parstatsall(pars[i],condition),dataframe)
    
  }
  return(dataframe)
}
allparstatsallL<- function(condition){
  pars<-asdparticipants()
  #browser()
  dataframe <- parstatsallL(pars[1],condition)
  for (i in 2: length(pars)){
    #dataframe <- rbindlist(list(parstatsallL(pars[i],condition),dataframe),fill= TRUE,use.names=TRUE, idcol=NULL)
    dataframe <- rbind(parstatsallL(pars[i],condition),dataframe)
  }
  return(dataframe)
}
allparstatsallR <- function(condition){
  pars<-asdparticipants()
  dataframe <- parstatsallR(pars[1],condition)
  for (i in 2: length(pars)){
    dataframe <- rbind(parstatsallR(pars[i],condition),dataframe)
    
  }
  return(dataframe)
}
allparstatsalltyp <- function(condition){
  pars<-typparticipants()
  dataframe <- parstatsall(pars[1],condition)
  for (i in 2: length(pars)){
    
    dataframe <- rbind(parstatsall(pars[i],condition),dataframe)
    
  }
  return(dataframe)
}
allparstatsalltypL <- function(condition){
  pars<-typparticipants()
  dataframe <- parstatsallL(pars[1],condition)
  for (i in 2: length(pars)){
    
    dataframe <- rbind(parstatsallL(pars[i],condition),dataframe)
    
  }
  return(dataframe)
}
allparstatsalltypR <- function(condition){
  pars<-typparticipants()
  dataframe <- parstatsallR(pars[1],condition)
  for (i in 2: length(pars)){
    
    dataframe <- rbind(parstatsallR(pars[i],condition),dataframe)
    
  }
  return(dataframe)
}

#_____Time to fixation functions_____#
TimetoFirstFixes <- function(dataframe, name, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2){
  newdataframe <- dataframe[,grep(name,dataframe[1,])]
  newdataframe <- newdataframe[-(1:23),]
  times <- vector("integer")
  
  if (ncol(newdataframe) == 0){
    sdataframe <- data.frame(NA,NA,NA)
    colnames(sdataframe) <- c("Xeye", "Yeye","Time")
    
  }
  #browser()
  if (ncol(newdataframe) != 0){
    for (i in seq(1,ncol(newdataframe),3)){
      #browser()
      if (i != ncol(newdataframe)){
        ddataframe <- newdataframe[,c(i,i+1,i+2)]
      }
      if (i == 1){
        #sdataframe <- getfirsttime(ddataframe, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2)
        sdataframe <- pickfirstFix(ddataframe, xbound1,xbound2,ybound1,ybound2)
        #browser()
        #may be slow?
        sdataframe <- rbind(sdataframe,pickfirstFix(ddataframe, xboundr1,xboundr2,yboundr1,yboundr2))
        colnames(sdataframe) <- c("Xeye", "Yeye","Time")
      }else{
        #browser()
        sdataframe <- rbind(sdataframe,pickfirstFix(ddataframe, xbound1,xbound2,ybound1,ybound2))
        sdataframe <- rbind(sdataframe,pickfirstFix(ddataframe, xboundr1,xboundr2,yboundr1,yboundr2))
      }
      
    }
  }
  #browser()
  return(sdataframe)
}
TimetoFirstFixesParallel <- function(dataframe, name, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2){
  newdataframe <- dataframe[,grep(name,dataframe[1,])]
  newdataframe <- newdataframe[-(1:23),]
  times <- vector("integer")
  #browser()
  if (ncol(newdataframe) == 0){
    sdataframe <- data.frame(NA,NA,NA)
    colnames(sdataframe) <- c("Xeye", "Yeye","Time")
    return(sdataframe)
    
  }
  ddataframe <- newdataframe[,c(1,2,3)]
  sdataframe <- getfirsttime(ddataframe, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2)
  sdataframe <- pickfirstFix(ddataframe, xbound1,xbound2,ybound1,ybound2)
  #may be slow?
  sdataframe <- rbind(sdataframe,pickfirstFix(ddataframe, xboundr1,xboundr2,yboundr1,yboundr2))
  colnames(sdataframe) <- c("Xeye", "Yeye","Time")
  #browser()
  if (ncol(newdataframe) != 0 && ncol(newdataframe) > 3){
    foreach(i = seq(1,ncol(newdataframe),3),.packages= 'dplyr') %dopar%{
      #browser()
      if (i != ncol(newdataframe)){
        ddataframe <- newdataframe[,c(i,i+1,i+2)]
      }
      if (i == 1){
        #sdataframe <- getfirsttime(ddataframe, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2)
        sdataframe <- pickfirstFix(ddataframe, xbound1,xbound2,ybound1,ybound2)
        #browser()
        #may be slow?
        sdataframe <- rbind(sdataframe,pickfirstFix(ddataframe, xboundr1,xboundr2,yboundr1,yboundr2))
        colnames(sdataframe) <- c("Xeye", "Yeye","Time")
      }else{
        #browser()
        sdataframe <- rbind(sdataframe,pickfirstFix(ddataframe, xbound1,xbound2,ybound1,ybound2))
        sdataframe <- rbind(sdataframe,pickfirstFix(ddataframe, xboundr1,xboundr2,yboundr1,yboundr2))
      }
      
    }
  }
  
  #browser()
  return(sdataframe)
}


TimetoFirstFix <- function(dataframe, name, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2){
  #browser()
  newdataframe <- dataframe[,grep(name,dataframe[1,])]
  newdataframe <- newdataframe[-(1:23),]
  times <- vector("integer")
  
  if (ncol(newdataframe) == 0){
    sdataframe <- data.frame(NA,NA,NA)
    colnames(sdataframe) <- c("Xeye", "Yeye","Time")
    
  }
  if (ncol(newdataframe) != 0){
    for (i in seq(1,ncol(newdataframe),3)){
      
      if (i != ncol(newdataframe)){
        ddataframe <- newdataframe[,c(i,i+1,i+2)]
      }
      if(name == "Marg.xlsm"){
        #browser()
      }
      if (i == 1){
        sdataframe <- getfirsttime(ddataframe, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2)
        colnames(sdataframe) <- c("Xeye", "Yeye","Time")
      }else{
        sdataframe <- rbind(sdataframe,getfirsttime(ddataframe, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2))
      }
      
    }
  }
  #browser()
  return(sdataframe)
}
TimetoFirstFixLR <- function(dataframe, name, xbound1,xbound2,ybound1,ybound2){
  
  newdataframe <- dataframe[,grep(name,dataframe[1,])]
  newdataframe <- newdataframe[-(1:23),]
  times <- vector("integer")
  
  if (ncol(newdataframe) == 0){
    sdataframe <- data.frame(NA,NA,NA)
    colnames(sdataframe) <- c("Xeye", "Yeye","Time")
    
  }
  if (ncol(newdataframe) != 0){
    for (i in seq(1,ncol(newdataframe),3)){
      ddataframe <- newdataframe[,c(i,i+1,i+2)]
      
      if (i == 1){
        sdataframe <- getfirsttime(ddataframe,xbound1,xbound2,ybound1,ybound2,xbound1,xbound2,ybound1,ybound2 )
        colnames(sdataframe) <- c("Xeye", "Yeye","Time")
      }else{
        sdataframe <- rbind(sdataframe,getfirsttime(ddataframe, xbound1,xbound2,ybound1,ybound2,xbound1,xbound2,ybound1,ybound2))
      }
      
    }
  }
  #browser()
  return(sdataframe)
}
Timestats <- function(dataframe, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2,AOI){
  pars <- asdparticipants()
  TimeToFixMean <- vector("integer") 
  TimeToFixStds <- vector("integer")
  #browser()
  for (i in 1: length(pars)) {
    
    #rawdata <- TimetoFirstFix(dataframe, pars[i], xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2)
    rawdata <- TimetoFirstFixes(dataframe, pars[i], xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2)
    #browser()
    TimeToFixMean[i]<- mean(as.numeric(unlist(rawdata[,3])),na.rm = TRUE)
    TimeToFixStds[i] <- sd(as.numeric(unlist(rawdata[,3])),na.rm=TRUE)
    #browser()
  }
  TimeToFixMean <- rev(TimeToFixMean)
  TimeToFixStds <- rev(TimeToFixStds)
  newdataframe <- data.frame(TimeToFixMean,TimeToFixStds)
  colnames(newdataframe) <- c(paste(colnames(newdataframe)[1], AOI, sep =""), paste(colnames(newdataframe)[2], AOI, sep =""))
  
  rownames(newdataframe)<-rev(pars)
  return(newdataframe)
}

Timestatstyp <- function(dataframe, xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2,AOI){
  pars <- typparticipants()
  TimeToFixMean <- vector("integer") 
  TimeToFixStds <- vector("integer")
  
  for (i in 1: length(pars)) {
    #browser()
    rawdata <- TimetoFirstFix(dataframe, pars[i], xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2)
    TimeToFixMean[i]<- mean(as.numeric(unlist(rawdata[,3])),na.rm = TRUE)
    TimeToFixStds[i] <- sd(as.numeric(unlist(rawdata[,3])),na.rm=TRUE)
    #browser()
  }
  TimeToFixMean <- rev(TimeToFixMean)
  TimeToFixStds <- rev(TimeToFixStds)
  newdataframe <- data.frame(TimeToFixMean,TimeToFixStds)
  colnames(newdataframe) <- c(paste(colnames(newdataframe)[1], AOI, sep =""), paste(colnames(newdataframe)[2], AOI, sep =""))
  
  rownames(newdataframe)<-rev(pars)
  return(newdataframe)
}
EveryAOItimeStat <- function(dataframe){
  cbind(Timestats(dataframe, 0.22565,0.35723,  0.42789, 0.5004,0.6411,	0.77268,	0.42789,	0.5004, "Eye"), 
        Timestats(dataframe, 0.2631,	0.32421,	0.51248,	0.58591,0.67856,	0.73967,	0.51248,	0.58591,"Nose"),
        Timestats(dataframe, 0.24832,	0.33506,	0.59707,	0.64912, 0.6687	,0.75544,	0.59707,	0.64912, "Mouth"),
        Timestats(dataframe, 0.17833,	0.39912,	0.20202,	0.42603, 0.59429,	0.81508,	0.20202,	0.42603, "Hair"),
        Timestats(dataframe,  0.19509,	0.3799,	0.65377,	0.74021,0.6209, 0.80571,	0.65377,	0.74021, "Jaw"))
}
EveryAOItimeStatL <- function(dataframe){
  cbind(Timestats(dataframe, 0.22565,0.35723,  0.42789, 0.5004, 0.22565,0.35723,  0.42789, 0.5004, "LFEye"), 
        Timestats(dataframe, 0.2631,	0.32421,	0.51248,	0.58591, 0.2631,	0.32421,	0.51248,	0.58591,"LFNose"),
        Timestats(dataframe, 0.24832,	0.33506,	0.59707,	0.64912,0.24832,	0.33506,	0.59707,	0.64912, "LFMouth"),
        Timestats(dataframe, 0.17833,	0.39912,	0.20202,	0.42603,0.17833,	0.39912,	0.20202,	0.42603,"LFHair"),
        Timestats(dataframe, 0.19509,	0.3799,	0.65377,	0.74021,0.19509,	0.3799,	0.65377,	0.74021, "LFJaw"))
}
EveryAOItimeStatR <- function(dataframe){
  cbind(Timestats(dataframe, 0.6411,	0.77268,	0.42789,	0.5004,0.6411,	0.77268,	0.42789,	0.5004, "RFEye"), 
        Timestats(dataframe, 0.67856,	0.73967,	0.51248,	0.58591,0.67856,	0.73967,	0.51248,	0.58591,"RFNose"),
        Timestats(dataframe,  0.6687	,0.75544,	0.59707,	0.64912, 0.6687	,0.75544,	0.59707,	0.64912, "RFMouth"),
        Timestats(dataframe, 0.59429,	0.81508,	0.20202,	0.42603,0.59429,	0.81508,	0.20202,	0.42603,"RFHair"),
        Timestats(dataframe, 0.6209, 0.80571,	0.65377,	0.74021,0.6209, 0.80571,	0.65377,	0.74021, "RFJaw"))
}

EveryAOItimeStattyp <- function(dataframe){
  cbind(Timestatstyp(dataframe, 0.22565,0.35723,  0.42789, 0.5004,0.6411,	0.77268,	0.42789,	0.5004, "Eye"), 
        Timestatstyp(dataframe, 0.2631,	0.32421,	0.51248,	0.58591,0.67856,	0.73967,	0.51248,	0.58591,"Nose"),
        Timestatstyp(dataframe, 0.24832,	0.33506,	0.59707,	0.64912, 0.6687	,0.75544,	0.59707,	0.64912, "Mouth"),
        Timestatstyp(dataframe, 0.17833,	0.39912,	0.20202,	0.42603, 0.59429,	0.81508,	0.20202,	0.42603, "Hair"),
        Timestatstyp(dataframe,  0.19509,	0.3799,	0.65377,	0.74021,0.6209, 0.80571,	0.65377,	0.74021, "Jaw"))
}
EveryAOItimeStatLtyp <- function(dataframe){
  cbind(Timestatstyp(dataframe, 0.22565,0.35723,  0.42789, 0.5004, 0.22565,0.35723,  0.42789, 0.5004, "LFEye"), 
        Timestatstyp(dataframe, 0.2631,	0.32421,	0.51248,	0.58591, 0.2631,	0.32421,	0.51248,	0.58591,"LFNose"),
        Timestatstyp(dataframe, 0.24832,	0.33506,	0.59707,	0.64912,0.24832,	0.33506,	0.59707,	0.64912, "LFMouth"),
        Timestatstyp(dataframe, 0.17833,	0.39912,	0.20202,	0.42603,0.17833,	0.39912,	0.20202,	0.42603,"LFHair"),
        Timestatstyp(dataframe, 0.19509,	0.3799,	0.65377,	0.74021,0.19509,	0.3799,	0.65377,	0.74021, "LFJaw"))
}
EveryAOItimeStatRtyp <- function(dataframe){
  cbind(Timestatstyp(dataframe, 0.6411,	0.77268,	0.42789,	0.5004,0.6411,	0.77268,	0.42789,	0.5004, "RFEye"), 
        Timestatstyp(dataframe, 0.67856,	0.73967,	0.51248,	0.58591,0.67856,	0.73967,	0.51248,	0.58591,"RFNose"),
        Timestatstyp(dataframe,  0.6687	,0.75544,	0.59707,	0.64912, 0.6687	,0.75544,	0.59707,	0.64912, "RFMouth"),
        Timestatstyp(dataframe, 0.59429,	0.81508,	0.20202,	0.42603,0.59429,	0.81508,	0.20202,	0.42603,"RFHair"),
        Timestatstyp(dataframe, 0.6209, 0.80571,	0.65377,	0.74021,0.6209, 0.80571,	0.65377,	0.74021, "RFJaw"))
}
getfirsttime <- function(dataframe,xbound1,xbound2,ybound1,ybound2, xboundr1,xboundr2,yboundr1,yboundr2){
  #browser()
  newdataframel <- dplyr::filter(dataframe, as.numeric(unlist(dataframe[3])) != -1)
  newdataframer <- dplyr::filter(dataframe, as.numeric(unlist(dataframe[3])) != -1)
  
  newdataframel <- filter(newdataframel, between(as.numeric(unlist(newdataframel[1])),xbound1,xbound2) & between(as.numeric(unlist(newdataframel[2])),ybound1,ybound2))
  newdataframer <- filter(newdataframer, between(as.numeric(unlist(newdataframer[1])),xboundr1,xboundr2) & between(as.numeric(unlist(newdataframer[2])), yboundr1, yboundr2))
  
  newdataframe <- rbind(newdataframel,newdataframer)
  colnames(newdataframe) <- c("Xeye", "Yeye","Time")
  return(newdataframe[1,])
}
#pick the first fixation point out of eye data
pickfirstFix <- function(dataframe,xbound1,xbound2,ybound1,ybound2){
  fixationCount <- 5
  inaRow <- FALSE
  inaRowCount <- 0
  xList <- as.numeric(unlist(dataframe[1]))
  yList <- as.numeric(unlist(dataframe[2]))
  xList <- xList[!is.na(xList)]
  yList <- yList[!is.na(yList)]
  colnames(dataframe) <-c("Xeye", "Yeye","Time")
  #browser()
  for (i in 1:length(xList)){
    if (is.na(xList[i])){
      break
    }
    if(between(xList[i],xbound1,xbound2) & between(yList[i],ybound1,ybound2)){
      inaRowCount <- inaRowCount + 1
    }else{
      inaRow <- FALSE
      inaRowCount <- 0
    }
    if (inaRowCount == fixationCount ){
      return(dataframe[i-4,])
      
    }
  }
  
  failure <- data.frame(NA,NA,NA)
  #browser()
  colnames(failure) <- c("Xeye", "Yeye","Time")
  return(failure)
}

#____Save data to Excel files____#
savedataI <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FF_Incorrect","FS_Incorrect","SF_Incorrect","I_Incorrect")
  # writetoExcel(list(incorrectstatsFF, incorrectstatsFS, incorrectstatsSF, incorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/ASD Stats/asdStats_Incorrect_FixationPoints.xls",conditions)
  writetoCSV(datalist, conditions)
}
savedataC <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FF_Correct","FS_Correct","SF_Correct","I_Correct")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/ASD Stats/asdStats_Correct_FixationPoints.xls",conditions)
  writetoCSV(datalist, conditions)
}
savedataA <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FF_All","FS_All","SF_All","I_All")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/ASD Stats/asdStats_All_FixationPoints.xls",conditions)
  writetoCSV(datalist, conditions)
}
savedataItyp <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FFtyp_Incorrect","FStyp_Incorrect","SFtyp_Incorrect","Ityp_Incorrect")
  # writetoExcel(list(incorrectstatsFF, incorrectstatsFS, incorrectstatsSF, incorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/Typ Stats/TypStats_Incorrect_FixationPoints.xls",conditions)
  writetoCSV(datalist, conditions)
}
savedataCtyp <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FFtyp_Correct","FStyp_Correct","SFtyp_Correct","Ityp_Correct")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/Typ Stats/TypStats_Correct_FixationPoints.xls",conditions)
  writetoCSV(datalist, conditions)
}
savedataAtyp <- function(dFF,dFS,dSF,dI){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FFtyp_All","FStyp_All","SFtyp_All","Ityp_All")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, "/Typ Stats/TypStats_All_FixationPoints.xls",conditions)
  writetoCSV(datalist, conditions)
}