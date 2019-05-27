
rm(list = ls())
#_____Required librairies_______#
require(foreach)
require(openxlsx)
require(dplyr)
require(rowr)
require(doParallel)
no_cores <- detectCores() - 1  
cl <- makeCluster(no_cores, type="FORK")  
registerDoParallel(cl)  
#require(data.table)

#_____Remove current workspace and load workspace with Master data______#

load(paste(getwd(), "Master RDA files_RG/FixationPoints.rda", sep = "/"))
load(paste(getwd(), "Master RDA files_RG/Masters.rda", sep = "/"))
#load(paste(getwd(), "Heatmapdata.rda", sep = "/"))
#_____Excel Writing functions______#
writetoExcel<- function(dataframes, file, conditions){
  #WriteXLS(data, file, condition, row.names = TRUE)
  #write.xlsx(data,file, sheetName = condition, row.names=TRUE, append = TRUE)
  wb <- createWorkbook()
  
  for (i in 1: length(conditions)){
    addWorksheet(wb, conditions[i])
  }
  #browser()
  for (j in 1: length(dataframes)){
    #browser()
    writeDataTable(wb, conditions[[j]], as.data.frame(dataframes[[j]]), rowNames = TRUE) 
    
  }
  saveWorkbook(wb, file, overwrite = TRUE)
  
}
writetoCSV <- function(dataframes, conditions){
  file = paste(getwd(), "/Condition CSV files_RG", sep = "")
  for (j in 1: length(conditions)){
    write.csv(dataframes[[j]], file = paste(file, paste(conditions[[j]], ".csv", sep = ""), sep ="/"), row.names= TRUE)
  }
}
writetoExcelone <- function(dataframe, file, sheetname, tablename){
  wb <- loadWorkbook(file)
  getTables(wb,sheet=sheetname)
  removeTable(wb=wb, sheet =sheetname, table = tablename)
  writeDataTable(wb,sheet = sheetname, dataframe, tableName = tablename,rowNames = TRUE)
  saveWorkbook(wb, file = file, overwrite = TRUE)
}



writetoCSVone <- function(dataframe, condition){
  file = paste(getwd(), "/Condition CSV files_RG", sep = "")
  write.csv(dataframe, file = paste(file, paste(condition, ".csv", sep = ""), sep ="/"), row.names= TRUE)
}

#_____Participant list functions_______#
#_____Remove Correct and Incorrect Data______#
#_____Note removeI actually keeps Incorrect data and vice versa for removeC______#
removeC<- function(dataframe){
  newdataframe = dataframe[,grep("Incorrect", dataframe[2,])]
  return(newdataframe)
}
removeI<- function(dataframe){
  newdataframe = dataframe[,is.na(unlist(dataframe[2,]))]
  return(newdataframe)
}

Transpose<- function(dataframe){
  numberAOIS <<- 11
  x <- data.frame(row=unique(list(colnames(dataframe))))
  colnames(x)<- unique(list(colnames(dataframe)))
  
  aoi <- c("eyeareaLF","eyeareaLF_time","eyeareRF","eyeareRF_time","NoseLF","NoseLF_time","NoseRF","NoseRF_time","MouthLF","MouseLF_time",
           "MouthRF","MouseRF_time","HairLF","HairLF_time","HairRF","HairRF_time","JawLF","JawLF_time","JawRF","JawRF_time","Total","Total_time")#"Answer","Speed")#"Result","EyeData")
  
  
  x <- vector("character")
  count <- 1
  for (j in 1:length(aoi)){
    for (i in 1:16){
      pasted <- paste("_t",i,sep="")
      x[count] <- paste(aoi[j], pasted, sep="")
      count <- count +1
    }
  }
  # x[length(x)+1] <- "Correct Answers"
  # x[length(x)+1] <- "False Answers"
  # x[length(x)+1] <- "% Correct"
  # x[length(x)+1] <- "Total Spee Average (s)"
  # x[length(x)+1] <- "Correct Speed Average (s)"
  # 
  #x[length(x)]<- "Participants"
  columns <- length(x)
  
  participants <- unique(unlist(dataframe[1,]))
  participants <- participants[!is.na(participants)]
  rows <- length(participants)
  
  df <- data.frame(matrix(ncol = columns, nrow = rows))
  
  colnames(df) <- x
  rownames(df)<- participants
  allvisualdata <- list(list())
  for ( i in 1:length(participants)){
    pardata <- dataframe[,grep(participants[i],dataframe[1,])]
    #put this into the new dataframe df. The rows contain the data we want
    #Get row number of the aois 
    a <- match(aoi,unlist(pardata[(1:23),1]))
    a <- unlist(append(a, list(x = 1),0))
    a <- unlist(append(a, list(x = 2),1))
    a <- a[!is.na(a)]
    
    seconds <- seq(2,ncol(pardata),3)
    thirds <- seq(3,ncol(pardata),3)
    
    stats <- pardata[a,seconds]
    timestats <- pardata[a,thirds]
    
    visualdata <- pardata[24:nrow(pardata),]
    trialno <- ncol(pardata)/3
    aoicolumns<-seq(1,trialno*numberAOIS*2, trialno)
    
    
    
    if (trialno > 1 ){
      df[participants[i],(1:trialno)] <- as.numeric(stats[3,(1:trialno)])
      df[participants[i],(17:(17+trialno-1))] <- as.numeric(timestats[3,(1:trialno)])
      
      df[participants[i],(33:(33+trialno-1))] <-as.numeric( stats[4,(1:trialno)])
      df[participants[i],(49:(49+trialno-1))] <- as.numeric(timestats[4,(1:trialno)])
      
      df[participants[i],(65:(65+trialno-1))] <- as.numeric(stats[5,(1:trialno)])
      df[participants[i],(81:(81+trialno-1))] <- as.numeric(timestats[5,(1:trialno)])
      
      df[participants[i],(97:(97+trialno-1))] <- as.numeric(stats[6,(1:trialno)])
      df[participants[i],(113:(113+trialno-1))] <- as.numeric(timestats[6,(1:trialno)])
      
      df[participants[i],(129:(129+trialno-1))] <- as.numeric(stats[7,(1:trialno)])
      df[participants[i],(145:(145+trialno-1))] <- as.numeric(timestats[7,(1:trialno)])
      
      df[participants[i],(161:(161+trialno-1))] <- as.numeric(stats[8,(1:trialno)])
      df[participants[i],(177:(177+trialno-1))] <- as.numeric(timestats[8,(1:trialno)])
      
      df[participants[i],(193:(193+trialno-1))] <- as.numeric(stats[9,(1:trialno)])
      df[participants[i],(209:(209+trialno-1))] <- as.numeric(timestats[9,(1:trialno)])
      
      df[participants[i],(225:(225+trialno-1))] <- as.numeric(stats[10,(1:trialno)])
      df[participants[i],(241:(241+trialno-1))] <- as.numeric(timestats[10,(1:trialno)])
      
      df[participants[i],(257:(257+trialno-1))] <- as.numeric(stats[11,(1:trialno)])
      df[participants[i],(273:(273+trialno-1))] <- as.numeric(timestats[11,(1:trialno)])
      
      df[participants[i],(289:(289+trialno-1))] <- as.numeric(stats[12,(1:trialno)])
      df[participants[i],(305:(305+trialno-1))] <- as.numeric(timestats[12,(1:trialno)])
      browser()
      df[participants[i],(321:(321+trialno-1))] <- as.numeric(stats[13,(1:trialno)])
      df[participants[i],(337:(337+trialno-1))] <- as.numeric(timestats[13,(1:trialno)])
    }
    if (trialno == 1){
      df[participants[i],(1:trialno)] <- as.numeric(stats[3])
      df[participants[i],(17:(17+trialno-1))] <- as.numeric(timestats[3])
      
      df[participants[i],(33:(33+trialno-1))] <-as.numeric( stats[4])
      df[participants[i],(49:(49+trialno-1))] <- as.numeric(timestats[4])
      
      df[participants[i],(65:(65+trialno-1))] <- as.numeric(stats[5])
      df[participants[i],(81:(81+trialno-1))] <- as.numeric(timestats[5])
      
      df[participants[i],(97:(97+trialno-1))] <- as.numeric(stats[6])
      df[participants[i],(113:(113+trialno-1))] <- as.numeric(timestats[6])
      
      df[participants[i],(129:(129+trialno-1))] <- as.numeric(stats[7])
      df[participants[i],(145:(145+trialno-1))] <- as.numeric(timestats[7])
      
      df[participants[i],(161:(161+trialno-1))] <- as.numeric(stats[8])
      df[participants[i],(177:(177+trialno-1))] <- as.numeric(timestats[8])
      
      df[participants[i],(193:(193+trialno-1))] <- as.numeric(stats[9])
      df[participants[i],(209:(209+trialno-1))] <- as.numeric(timestats[9])
      
      df[participants[i],(225:(225+trialno-1))] <- as.numeric(stats[10])
      df[participants[i],(241:(241+trialno-1))] <- as.numeric(timestats[10])
      
      df[participants[i],(257:(257+trialno-1))] <- as.numeric(stats[11])
      df[participants[i],(273:(273+trialno-1))] <- as.numeric(timestats[11])
      
      df[participants[i],(289:(289+trialno-1))] <- as.numeric(stats[12])
      df[participants[i],(305:(305+trialno-1))] <- as.numeric(timestats[12])
      
      df[participants[i],(321:(321+trialno-1))] <- as.numeric(stats[13])
      df[participants[i],(337:(337+trialno-1))] <- as.numeric(timestats[13])
    }
    #df[participants[i],(353:(353+trialno-1))] <- (stats[2,(1:trialno)])
    
    # speeds <- (pardata[22,])[!is.na(pardata[22,])]
    # speeds <- speeds[!speeds %in% "Speed"]
    # if (length(speeds) != 0 ){
    #   df[participants[i], (369:(369+trialno-1))] <- speeds
    # }
    # df[,"No. Correct"] <- as.numeric(pardata[15,2])
    
    # visualdata2<- vector()
    # visualdata2 <- list(visualdata[,(1:3)],visualdata[,(4:6)],visualdata[,(7:9)],visualdata[,(10:12)],visualdata[,(13:15)],
    #                  visualdata[,(16:18)],visualdata[,(19:21)],visualdata[,(22:24)],visualdata[,(25:27)],visualdata[,(28:30)],
    #                  visualdata[,(31:33)],visualdata[,(34:36)],visualdata[,(37:39)],visualdata[,(40:42)],visualdata[,(43:45)],
    #                  visualdata[,(46:48)])
    # 
    # 
    # df[participants[i],(385:400)]
    # allvisualdata[participants[i]] <- visualdata2
    
    #df[participants[i],(401:416)] <- visualdata2
    
    
    
  }
  
  return(df)
  #Last here 2019-05-091:55:31 PM
}
filtervisuals <- function(visuals){
  
  new <- dplyr::filter(visuals, as.numeric(unlist(visuals[[3]])) != -1)
  totalpoints <- nrow(new)
  new <- dplyr::filter(new, as.numeric(unlist(new[[2]])) != -1)
  new <- dplyr::filter(new, as.numeric(unlist(new[[1]])) != -1)
  totalpoints2 <- nrow(new)
  if (totalpoints == 0 ){
    print("More than 80% of data has been removed! Beware")
  }else if((totalpoints2/totalpoints) < .2){
    print("More than 80% of data has been removed! Beware")
  }
  
  return(new)
}
visualdata_gen <- function(dataframe){
  
  participants <- unique(unlist(dataframe[1,]))
  participants <- participants[!is.na(participants)]
  
  visualdata_all <- list()
  
  for (i in 1: length(participants)){
    
    pardata <- dataframe[,grep(participants[i],dataframe[1,])]
    if( !is.null(nrow(pardata))){
      #browser()
      visualdata <- pardata[24:nrow(pardata),]
      
      # visualdata2 <- list(dplyr::filter(visualdata[,(1:3)],as.numeric(unlist(visualdata[,(1:3)][[3]])) !=-1),
      #                     dplyr::filter(visualdata[,(4:6)],as.numeric(unlist(visualdata[,(4:6)][[3]])) !=-1),
      #                     dplyr::filter(visualdata[,(7:9)],as.numeric(unlist(visualdata[,(7:9)][[3]])) !=-1),
      #                     dplyr::filter(visualdata[,(10:12)],as.numeric(unlist(visualdata[,10:12][[3]]))!=-1),
      #                     filtervisuals(visualdata[,(13:15)]),
      #                     filtervisuals(visualdata[,(16:18)]),
      #                     filtervisuals(visualdata[,(19:21)]),
      #                     filtervisuals(visualdata[,(22:24)]),
      #                     filtervisuals(visualdata[,(25:27)]),
      #                     filtervisuals(visualdata[,(28:30)]),
      #                     filtervisuals(visualdata[,(31:33)]),
      #                     filtervisuals(visualdata[,(34:36)]),
      #                     filtervisuals(visualdata[,(37:39)]),
      #                     filtervisuals(visualdata[,(40:42)]),
      #                     filtervisuals(visualdata[,(43:45)]),
      #                     filtervisuals(visualdata[,(46:48)]))
      visualdata2 <- list()
      for (j in seq(1,ncol(visualdata),3)){
        newdf <- as.data.frame(filtervisuals(visualdata[,(j:(j+2))]))
        # colnames(newdf) <- pardata[2,(j:(j+2))]
        visualdata2[[length(visualdata2)+1]]<- newdf
        #transform(visualdata2[length(visualdata2)], visualdata2[]= as.numeric(char))
      }
      visualdata_all[[participants[i]]]<- visualdata2
    }
  }
  return(visualdata_all)
}

TimeToFirst<- function(init_dataframe, visualdata,aoicoords,aoi){
  
  participants <- rownames(init_dataframe)
  dataframe<-as.data.frame(init_dataframe)
  if (length(participants) == 0){
    print("No participants. Ensure that the dataframe being passed is the stats dataframe")
    return(NULL)
  }
  # aoi<- c("eyeareaLF", "eyeareRF","Eye","NoseLF","NoseRF","Nose","MouthLF",
  #         "MouthRF","Mouth","HairLF","HairRF","Hair","JawLF","JawRF","Jaw")#"Result","EyeData")
  for (k in 1:length(participants)){
    data <- visualdata[k][[1]]
    for (j in 1:length(aoi)){
      coordcount <- j
      store <- vector()
      store2<- vector()
      store3<- vector()
      for (i in 1:length(data)){
        #browser()
        # dataframe[,paste(aoi[j], paste("_FirstGazeTime_",i,sep=""), sep="")] <- NA
        # dataframe[,paste(aoi[j+1], paste("_FirstGazeTime_",i,sep=""), sep="")] <- NA
        # dataframe[,paste(aoi[j+2], paste("_FirstGazeTime_",i,sep=""), sep="")] <- NA
        #browser()
        #Get eye data
        data2 <- data[[i]]
        #browser()
        #Fukter ete data
        FFLeye<-filter(data2,between(as.numeric(unlist(data2[1])),as.numeric(aoicoords[[coordcount]][1]),as.numeric(aoicoords[[coordcount]][2])))
        FFLeye<-filter(FFLeye,between(as.numeric(unlist(FFLeye[[2]])),as.numeric(aoicoords[[coordcount]][3]), as.numeric(aoicoords[[coordcount]][4])))
        # FFReye<-filter(data2,between(as.numeric(unlist(data2[1])),as.numeric(aoicoords[[coordcount]][5]),	as.numeric(aoicoords[[coordcount]][6])))
        # FFReye<-filter(FFReye,between(as.numeric(unlist(FFReye[[2]])),as.numeric(aoicoords[[coordcount]][7]),	as.numeric(aoicoords[[coordcount]][8])))
        FirstLeye<-as.numeric(FFLeye[1,3])
        # FirstReye<-as.numeric(FFReye[1,3])
        # FirstEye <-as.numeric(min(FFLeye[1,3],FFReye[1,3]))
        #browser()
        #Add Columns
        dataframe[participants[k],paste(aoi[j], paste("_FirstGazeTime_",i,sep=""), sep="")] <- FirstLeye
        store[length(store)+1]<-FirstLeye
        
        # dataframe[participants[k],paste(aoi[j+1], paste("_FirstGazeTime_",i,sep=""), sep="")] <- FirstReye
        # store2[length(store)+1]<-FirstReye
        # dataframe[participants[k],paste(aoi[j+2], paste("_FirstGazeTime_",i,sep=""), sep="")] <- FirstEye
        # store3[length(store)+1]<-FirstEye
        
        
      }
      
      #Add means
      
      dataframe[participants[k],paste(aoi[j], paste("_FirstGazeTime_","Mean",sep=""), sep="")] <- mean(store,na.rm = TRUE)
      dataframe[participants[k],paste(aoi[j], paste("_FirstGazeTime_","Sd",sep=""), sep="")] <- sd(store,na.rm = TRUE)
      
      # dataframe[participants[k],paste(aoi[j+1], paste("_FirstGazeTime_","Mean",sep=""), sep="")] <- mean(store2, na.rm=TRUE)
      # dataframe[participants[k],paste(aoi[j+1], paste("_FirstGazeTime_","Sd",sep=""), sep="")] <- sd(store2,na.rm= TRUE)
      # 
      # dataframe[participants[k],paste(aoi[j+2], paste("_FirstGazeTime_","Mean",sep=""), sep="")] <- mean(store3,na.rm = TRUE)
      # dataframe[participants[k],paste(aoi[j+2], paste("_FirstGazeTime_","Sd",sep=""), sep="")] <- mean(store3,na.rm = TRUE)
      
    }
  }
  return(dataframe)
}

Statistics <- function(init_dataframe,visualdata){
  participants <- rownames(init_dataframe)
  dataframe <- as.data.frame(init_dataframe)
  
  aoi <- c("eyeareaLF","eyeareaLF_time","eyeareRF","eyeareRF_time","NoseLF","NoseLF_time","NoseRF","NoseRF_time","MouthLF","MouseLF_time",
           "MouthRF","MouseRF_time","HairLF","HairLF_time","HairRF","HairRF_time","JawLF","JawLF_time","JawRF","JawRF_time","Total","Total_time")
  dataframe<-dataframe %>% select_if(is.numeric)
  
  count <- 1
  index <- seq(1,ncol(dataframe),16)
  
  for( j in index){
    par2<-dataframe[,(j:(j+(16-1)))]
    
    pointmeans<-rowMeans(par2, na.rm= TRUE)
    pointsds<-apply(par2,1,sd,na.rm= TRUE)
    # timemeans<-rowMeans(par,na.rm=TRUE)
    # timesds<-apply(par,1,sd)
    dataframe[,paste(aoi[count],"_Mean",sep = "")] <- pointmeans
    dataframe[,paste(aoi[count],"_Sd",sep = "")] <- pointsds
    
    # dataframe[,paste(aoi[j-15],"_Time_Mean",sep = "")] <- timemeans
    # dataframe[,paste(aoi[j-15],"_Time_Sd",sep = "")] <- timesds
    count <- count + 1
  }
  
  return(dataframe)
}
Statistics2 <- function(visualdata,aois,type){
  
  
  aois2 <- vector("character")
  count <- 1
  
  for (j in 1:length(aois)){
    for (i in 1:16){
      pasted <- paste("_t",i,sep="")
      aois2[count] <- paste(aois[[j]][5], pasted, sep="")
      count <- count +1
    }
  }
  
  pars <- unique(unlist(names(visualdata)))
  pars <- pars[!is.na(pars)]
  
  df<- data.frame(matrix(ncol = length(aois2), nrow = length(pars)))
  rownames(df)<- pars
  colnames(df)<- aois2
  
  for (i in 1: length(pars)){
    data <- visualdata[pars[i]][[1]]
    if(type == "Correct" || type == "All"){
      correctno <- length(data)
    }
    if(type == "Incorrect"){
      incorrectno <- length(data)
    }
    
    speedcount <- 1
    speeds <- vector()
    #go thru each aoi
    for (aoicount in 1: length(aois)){
      #go thru each trial
      
      for (k in 1: length(data)){
        new <- dplyr::filter(data[[k]], between(as.numeric(unlist(data[[k]][1])), as.numeric(aois[[aoicount]][1]), as.numeric(aois[[aoicount]][2])))
        new <- dplyr::filter(new, between(as.numeric(unlist(new[2])), as.numeric(aois[[aoicount]][3]), as.numeric(aois[[aoicount]][4])))
        
        #go to the participant row and the column
        df[pars[i], paste(paste(aois[[aoicount]][5],"_t",sep=""),k,sep="")] <- nrow(new)
        df[pars[i], paste(paste(paste(aois[[aoicount]][5],"_time",sep=""),"_t",sep=""),k,sep="")] <- nrow(new)*0.008333348
        df[pars[i], paste("Total_t",k, sep="")] <- nrow(data[[k]])
        df[pars[i], paste("Total_time_t",k, sep="")] <- nrow(data[[k]])*.008333348
        if (aoicount == 1 ){
          df[pars[i], paste("Speed_t",k,sep="")] <- max(as.numeric(data[[k]][,3]))
          speeds[speedcount] <- max(as.numeric(data[[k]][,3]))
          speedcount <- speedcount+1
        }
      }
      browser()
      
      
      #Last here 2019-05-214:43:49 PM
    }
    
    if(type == "Correct"){
      df[pars[i], "Correct"] <- correctno
    }
    if(type == "Incorrect"){
      df[pars[i], "Incorrect"] <- incorrectno
    }
    
    # if(type == "All"){
    #   df[pars[i], "All"] <- correctno
    # }
    df[pars[i], paste(type,"_Speed_Avg", sep = "")] <- mean(speeds,na.rm = TRUE)
    df[pars[i], paste(type,"_Speed_Avg", sep = "")] <- sd(speeds,na.rm=TRUE)
    
    
  }
  
  return(df)
}
#Types: ASD,TYP
AllStats <- function(alldata,type){
  browser()
  if (type =="ASD"){
    df<-alldata[grepl("A0",rownames(alldata)),]
  }
  if(type == "TYP"){
    df<-alldata[!grepl("A0",rownames(alldata)),]
  }
  if(type == "ALL"){
    df <- alldata
  }
  means<- list(as.numeric(unlist(colMeans(alldata,na.rm=TRUE))))[[1]]
  sds <- list(as.numeric(unlist(apply(df,2,sd,na.rm= TRUE))))[[1]]
  df["Mean",] <-means
  df["Standard Deviation",] <-sds
  return(df)
}
#____Save data to Excel files____#
savedataA <- function(dFF,dFS,dSF,dI,name){
  datalist <- list(dFF,dFS,dSF,dI)
  conditions <- list("FF_All","FS_All","SF_All","I_All")
  # writetoExcel(list(CorrectstatsFF, CorrectstatsFS, CorrectstatsSF, CorrectstatsI), "instatsrawdata.xls", conditions)
  writetoExcel(datalist, paste(getwd(),name,sep="/"),conditions)
  writetoCSV(datalist, conditions)
}

#remove all NA columns
FF1[sapply(FF1, function(x) all(is.na(x)))] <- NULL
FF2[sapply(FF2, function(x) all(is.na(x)))] <- NULL
FS1[sapply(FS1, function(x) all(is.na(x)))] <- NULL
FS2[sapply(FS2, function(x) all(is.na(x)))] <- NULL
SF1[sapply(SF1, function(x) all(is.na(x)))] <- NULL
SF2[sapply(SF2, function(x) all(is.na(x)))] <- NULL
I1[sapply(I1, function(x) all(is.na(x)))] <- NULL
I2[sapply(I2, function(x) all(is.na(x)))] <- NULL

#Turn into FixationPoint Data
FFG1[sapply(FFG1, function(x) all(is.na(x)))] <- NULL
FFG2[sapply(FFG2, function(x) all(is.na(x)))] <- NULL
FSG1[sapply(FSG1, function(x) all(is.na(x)))] <- NULL
FSG2[sapply(FSG2, function(x) all(is.na(x)))] <- NULL
SFG1[sapply(SFG1, function(x) all(is.na(x)))] <- NULL
SFG2[sapply(SFG2, function(x) all(is.na(x)))] <- NULL
IG1[sapply(IG1, function(x) all(is.na(x)))] <- NULL
IG2[sapply(IG2, function(x) all(is.na(x)))] <- NULL

FF1<-rbind(FFG1,FF1)[-(14:28),]
FF2<-rbind(FFG2,FF2)[-(14:28),]
FS1<-rbind(FSG1,FS1)[-(14:28),]
FS2<-rbind(FSG2,FS2)[-(14:28),]
SF1<-rbind(SFG1,SF1)[-(14:28),]
SF2<-rbind(SFG2,SF2)[-(14:28),]
I1<-rbind(IG1,I1)[-(14:28),]
I2<-rbind(IG2,I2)[-(14:28),]
# 
# FF1 <- removeI(FF1)
# FF2<- removeI(FF2)
# SF1 <- removeI(SF1)
# SF2 <- removeI(SF2)
# FS1 <- removeI(FS1)
# FS2 <- removeI(FS2)
# I1 <- removeI(I1)
# I2 <- removeI(I2)

library(foreach)
library(doParallel)
registerDoParallel(4)


# Get visualdata
conditions <- list(FF1,FF2,FS1,FS2,SF1,SF2,I1,I2)
conditions <- list(cbind(FF1,FF2),cbind(FS1,FS2),cbind(SF1,SF2),cbind(I1,I2))
allvisualdata_all <- list()

#remove incorrect
aoifile <- paste(getwd(),"/McgillStuff/aoi.csv",sep = "")
aoinamesfile <- paste(getwd(),"/McgillStuff/aoinames.csv",sep = "")
aoiscsv <- read.csv(aoifile)
aoinames <- read.csv(aoinamesfile)
aois <- list()
for (i in 1:nrow(aoiscsv)){
  def <- ""
  if (is.null(aoinames)){
    def <- paste("AOI", i , sep ="")
  }else{
    def <- names(aoinames[i])
  }
  # aois[[i]] <- c(aoiscsv[i,1], aoiscsv[i,1]+aoiscsv[i,3], aoiscsv[i,2], aoiscsv[i,2]+aoiscsv[i,3],def)
  aois[[i]] <- c(aoiscsv[i,1],aoiscsv[i,2],aoiscsv[i,3],aoiscsv[i,4],def)
}
# aois <- list(c(0.22565,0.35723,  0.42789, 0.5004,"LEye"),
#              c(0.6411,	0.77268,	0.42789,	0.5004, "REye"),
#              c(0.2631,	0.32421,	0.51248,	0.58591,"NoseLF"),
#              c(0.67856,	0.73967,	0.51248,	0.58591,"NoseRF"),
#              c(0.24832,	0.33506,	0.59707,	0.64912,"MouthLF"),
#              c(0.6687	,0.75544,	0.59707,	0.64912, "MouthRF"),
#              c(0.17833,	0.39912,	0.20202,	0.42603,"HairLF"),
#              c(0.59429,	0.81508,	0.20202,	0.42603, "HairRF"),
#              c(0.19509,	0.3799,	0.65377,	0.74021,"JawLF"),
#              c(0.6209, 0.80571,	0.65377,	0.74021, "JawRF"))
# aoi<- c("LEye", "REye","NoseLF","NoseRF","MouthLF",
#         "MouthRF","HairLF","HairRF","JawLF","JawRF")

system.time(allvisualdata_all <- foreach (i = 1:length(conditions)) %dopar%{
  x <- visualdata_gen(conditions[[i]])
  x
})
# system.time(master_data <- foreach (i = 1:length(conditions)) %dopar%{
#   x <- Transpose(conditions[[i]])
#   x
# })
#Area of Interest Stats: time to first, mean time, mean points
# system.time(stats <- foreach (i = 1:length(conditions)) %dopar%{
#   x <- Statistics(master_data[[i]])
#   x
# })
system.time(statsnondefault <- foreach(i = 1:length(conditions)) %dopar%{ 
  x <- Statistics2(allvisualdata_all[[i]],aois,"All")
  #Last here 2019-05-208:32:18 PM
  #Test if time to fix works aas well.
  x
})



# system.time(timetoFix <- foreach(i = 1:length(conditions)) %dopar%{ 
#   x <- TimeToFirst(stats[[i]],allvisualdata_all[[i]],aois)
#   
#   x
# })

# 
# for ( i in 1:16){
#   # new[[i]] <- c(paste(aoi[1], paste("_t",i,sep="")),paste(aoi[2], paste("_t",i,sep="")),paste(aoi[3], paste("_t",i,sep="")),paste(aoi[4], paste("_t",i,sep="")),
#   #               paste(aoi[5], paste("_t",i,sep="")),paste(aoi[6], paste("_t",i,sep="")),paste(aoi[7], paste("_t",i,sep="")),paste(aoi[8], paste("_t",i,sep="")),
#   #               paste(aoi[9], paste("_t",i,sep="")),paste(aoi[10], paste("_t",i,sep="")))
#   for (j in 1:length(aoi)){
#     new[[i]][j] <- paste(aoi[j], paste("_t",i,sep=""),sep="")
#   }
# }
# for (i in 1:16){
#   df[,new[[i]]] 
# }

system.time(timetoFix <- foreach(i = 1:length(conditions)) %dopar%{ 
  x <- TimeToFirst(statsnondefault[[i]],allvisualdata_all[[i]],aois,aoi)
  x
})

system.time(newtimetoFix <- foreach(i = 1:length(conditions)) %dopar%{ 
  x <- AllStats(timetoFix[[i]],"ALL")
  x
})
system.time(newtimetoFixTYP <- foreach(i = 1:length(conditions)) %dopar%{ 
  x <- AllStats(timetoFix[[i]],"TYP")
  x
})
system.time(newtimetoFixASD <- foreach(i = 1:length(conditions)) %dopar%{ 
  x <- AllStats(timetoFix[[i]],"ASD")
  x
})




savedataA(newtimetoFix[[1]],newtimetoFix[[2]],newtimetoFix[[3]],newtimetoFix[[4]],"Stats_All3.xls")
savedataA(newtimetoFixTYP[[1]],newtimetoFixTYP[[2]],newtimetoFixTYP[[3]],newtimetoFixTYP[[4]],"Stats_TYP3.xls")
savedataA(newtimetoFixASD[[1]],newtimetoFixASD[[2]],newtimetoFixASD[[3]],newtimetoFixASD[[4]],"Stats_ASD3.xls")
save(newtimetoFix,newtimetoFixTYP,newtimetoFixASD,file = paste(getwd(),"/Master RDA files_RG/HeatmapdataA.rda",sep=""))
