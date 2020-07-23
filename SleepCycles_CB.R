##################################################################################################################################
## Code written to analyse sleep cycles sleep scoring data    
##                                                                      
## Author: CBlume                                                       
## Date: 22 07 20
## Version: 1.0
##
## The function requires any sleep staging file with a column named "Description", in which the sleep stages are coded 
## in the usual 0,1,2,3,5 (i.e., W, N1, N2, N3, REM) pattern. Staging must be in 30s epochs. Besides text files, it 
## can also handle marker files for the Brain Vision Analyzer (filetype = "txt" (default), "vmrk", or "vpd").
##
## Sleep cycles are largely defined according to the originally proposed criteria by Feinberg & Floyd (1979). 
## NREM periods are periods starting with N1 with a minimal duration of 15min (can include W, but not REM). 
## REM following a NREM period automatically starts a potential REM period, however any REMP must be at least
## 5min (except the first REMP). NREMPs that exceed 120min in duration (excl. wake) can be split into 2 parts. 
## The new cycle then starts with the first N3 episode following a phase (>12min) with any other stage than N3 
## (cf. Rudzik et al., 2020; Jenni et al., 2004; Kurth et al., 2010).
##
## Besides sleep cycles (NREM-REM), the result also splits the NREM and REM parts of each cycle in percentiles. In case the 
## length of a period is not divisible by 10 (i.e., 203 epochs), we added one epoch to percentiles in a randomized
## fashion to reach the correct length of a period (i.e., 7 percentiles comprised 20 epochs, 3 comprised 21).
## 
## The code offers to choose whether incomplete cycles should be removed at the end of the night (rm_incompletecycs, default = F). 
##
## The user can either process all files in a given directory (default) or specific files by specifying a vector of files.
##
## By default, the function produces and saves a plot for visual inspection of the results.
################################################################################################################################## 
SleepCycles_CB <- function(p, files = NA, filetype = "txt", rm_incompletecycs = F, plot = T){
  
  # # --- set a few things
  setwd(p)
  
  #----- check if pacman is installed - if not install it
  if(!require(pacman)) install.packages("pacman")
  
  #----- use pacman function p_load to check all packages that you are using in this script
  pacman::p_load(ggplot2, reshape2, plyr, stringr, viridis)
  
  #----- list all files in folder
  if (filetype == "vmrk"){
    d <- list.files(p, pattern = "*.vmrk")
  }else if (filetype == "vpd"){
#################################################################
  }else if (filetype == "txt"){
    d <- list.files(p, pattern = "*.txt")
  }
  
  #----- has a vector of files been specified?
  if (!all(is.na(files))){
    d <- d[files]
  }
  
  #--------------------------------------------------------
  #----- loop through files in directory to determine sleep cycles
  #--------------------------------------------------------
  
  for (i in 1:length(d)){
    print(i)
    filename <- d[i]
    if (filetype == "vmrk"){
      header <- read.csv(filename, nrows = 1, header = F)
      data <- read.csv(filename, skip = 1) #each sleep stage refers to the 30s preceding the marker (irrespective of SR!)
    }
    
    cycles <- data
    cycles[,1] <- "SleepCycle"
    cycles[,2] <- NA
    
    # Recode/combine stages
    data$Descr2 <- NA
    data$Descr2[data$Description == 1 | data$Description == 2 | data$Description == 0 | data$Description == 5] <- "RWN12"
    data$Descr2[data$Description == 3] <- "N3"
    data$Descr3 <- NA
    data$Descr3[data$Description == 1 | data$Description == 2 | data$Description == 3] <- "NREM"
    data$Descr3[data$Description == 0] <- "W"
    data$Descr3[data$Description == 5] <- "REM"
    
    ## Find NREM periods: start with N1 and can then also include W. >=15min
    NREMWs <- which(data$Descr3 == "NREM"| data$Descr3 == "W") #which 30s epochs are NREM or wake
    NREMs <- which(data$Descr3 == "NREM")
    NREMWs <- subset(NREMWs, NREMWs >= NREMs[1]) # exclude W at the beginning of the night
    
    ## Loop through NREMWs 
    ## check if the sequence of NREWM is continuous and the period is >=15min AND beginning is not wake
    NREMWs_start <- NA
    for (k in 1:(length(NREMWs)-29)){
      if ((all(seq(NREMWs[k],length.out = 30) == NREMWs[seq(k,k+29)])) & (data$Descr3[NREMWs[k]] != "W")){ 
        NREMWs_start <- c(NREMWs_start, NREMWs[k])
      }else{
        next
      }
    }
    NREMWs_start <- NREMWs_start[-c(1)] #first was NA, remove
    
    # find discontinuities in the sequence (= potential beginnings of new NREM period)
    NREMWs_start2 <- NREMWs_start[1]
    for (k in 1:(length(NREMWs_start)-1)){
      if ((NREMWs_start[k+1]-NREMWs_start[k])>1){
        NREMWs_start2 <- c(NREMWs_start2, NREMWs_start[k+1]) #if there is a discontinuity in the sequence, mark the beginning of a new NREM period
      }
    }
    data$CycleStart <- NA
    data$CycleStart[NREMWs_start2] <- "NREMP"
    
    ## Find REM episodes (first can be <5min, others have to be at least 5min)
    REMs <- which(data$Descr3 == "REM") #which 30s epochs are NREM
    REMs_start <- REMs[1] #set beginning of first as no criterion
    for (k in 1:(length(REMs)-9)){
      if (all(seq(REMs[k],length.out = 10) == REMs[seq(k,k+9)])){ # check if the sequence of NREM epochs is continuous
        REMs_start <- c(REMs_start, REMs[k])
      }
    }
    REMs_start <- unique(REMs_start)
    
    REMs_start2 <- REMs_start[1] 
    for (k in 1:(length(REMs_start)-1)){
      if ((REMs_start[k+1]-REMs_start[k])>1){
        REMs_start2 <- c(REMs_start2, REMs_start[k+1]) #if there is an discontinuity in the sequence, mark the beginning of a new NREM sequence
      }
    }
    data$CycleStart[REMs_start2] <- "REMP"
    
    ## remove several NREMPs or REMPs in a row
    rm <- NA
    cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
    for (k in 2:length(cycs)){
      if(data$CycleStart[cycs[k]] == data$CycleStart[cycs[k-1]])
        rm <- c(rm, cycs[k])
    }
    rm <- unique(rm)
    rm <- rm[c(-1)]
    data$CycleStart[c(rm)] <- NA
    
    ## is any NREM part (excl. wake) of a NREMP longer than 120min?
    cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
    
    toolong <- NA
    for (k in seq(2,length(cycs),2)){
      subset <- data[c(cycs[k-1]:(cycs[k]-1)),]
      wake_eps <- sum(subset$Descr2 == "W")
      if (((cycs[k]-cycs[k-1])-wake_eps)>=240){ #<= as cycs[k] is already the beginning of the REMP
        toolong <- c(toolong, cycs[k-1])
      }
    }
    
    # now split this NREMP
    if (length(toolong) > 1){
      toolong <- toolong[c(-1)] #rm NA from beginning
      
      for (k in (1:length(toolong))){
        beg_end <- c(cycs[which(cycs==toolong[k])], cycs[which(cycs==toolong[k])+1]) #find beginning and end of NREMP that is too long
        
        # find RWN12 episodes that are > 12min
        RWN12s <- which(data$Descr2 == "RWN12") #which 30s epochs are N1/2
        RWN12s <- RWN12s[c(RWN12s>=beg_end[1] & RWN12s<=beg_end[2])]
        RWN12s_start <- NA
        for (kk in 1:(length(RWN12s)-23)){
          if (all(seq(RWN12s[kk],length.out = 24) == RWN12s[seq(kk,kk+23)])){ # check if the sequence of N12 epochs is continuous
            RWN12s_start <- c(RWN12s_start, RWN12s[kk])
          }
        }
        RWN12s_start <- RWN12s_start[-c(1)] #first was NA, remove
        rm(kk)
        
        RWN12s_start2 <- RWN12s_start[1]
        for (kk in 1:(length(RWN12s_start)-1)){
          if ((RWN12s_start[kk+1]-RWN12s_start[kk])>1){
            RWN12s_start2 <- c(RWN12s_start2, RWN12s_start[kk+1]) #if there is an discontinuity in the sequence, mark the beginning of a new NREM sequence
          }
        }
        
        # find first N3 episode
        N3s_1 <- which(data$Descr2 == "N3")[1] #which is the first 30s epoch of N3
        
        # delete RWN12s_start2 before first N3s
        RWN12s_start2 <- RWN12s_start2[c(RWN12s_start2>N3s_1)]
        
        # second NREMP starts with N3 following 12min of N1/2
        N3s <- which(data$Descr2 == "N3") #which 30s epoch of N3 are there
        N3s_cycstart <- N3s[c(N3s > RWN12s_start2[1])][1] # gives first N3 epoch after N12 episode >12min
        data$CycleStart[N3s_cycstart] <- "NREMP"
        
        if (k == 1){
          splits <- N3s_cycstart
        }else{
          splits <- c(splits, N3s_cycstart)
        }
      }
      splits <- unique(splits)
      
      # ask user if s/he is happy with the result of the splitting
      # plot results
      dfplot <- data
      dfplot <- dfplot[,-c(1,4,5)]
      dfplot$time <- seq(1, nrow(dfplot)) # gives epochs
      dfplot$Description[dfplot$Description == 1] <- -1
      dfplot$Description[dfplot$Description == 2] <- -2
      dfplot$Description[dfplot$Description == 3] <- -3
      dfplot$Description[dfplot$Description == 5] <- 1
      
      p <- ggplot(dfplot, aes(x=time, y=Description, colour=Description)) 
      p <- p + theme_bw()+
        geom_point() +
        geom_line(aes(x=time, y=Description))+
        xlab("Time") +
        ylab("Sleep Stage")+
        scale_y_continuous(limits = c(-3,2), breaks = c(-3, -2, -1, 0, 1), labels = c("N3", "N2", "N1", "W", "REM"))+
        scale_color_viridis(name = "Sleep Stage", option = "D")+
        geom_vline(xintercept = c(splits), lty = 2, colour = "red")+
        annotate(geom="text", x = 500, y = 2, label = paste("split at epoch(s):", as.character(splits), sep = " "))
      print(p)
      
      val <- readline("Are you happy with the result (y/n/skip)?. ") 
      
      if(val == "skip"){
        message("This night is skipped.")
        next
      }else if (val == "n"){
        newperiod <- readline("At which epoch do you want to start the new NREM period instead? Please type epoch number. ") 
        data$CycleStart[N3s_cycstart] <- NA #rm old indicator
        data$CycleStart[as.numeric(newperiod)] <- "NREMP"
      }else if (val == "y"){
        next
      }else{
        message("Missing entry. This night is skipped")
        next
      }
    }
    rm(dfplot, p)
    
    # -----------------------------------------------------------------------------
    # now finish and add cycle markers and percentiles
    
    # add cycle markers to file (only for NREM cycles)
    NREMPs <- which(data$CycleStart == "NREMP") #where do NREMPs begin?
    
    data$cycles <- NA
    for (k in 1:(length(NREMPs))){
      if (k<length(NREMPs)){
        data$cycles[NREMPs[k]:(NREMPs[k+1]-1)] <- k 
      }else{
        data$cycles[NREMPs[k]:(nrow(data))] <- k 
      }
    }
    
    # add NREM vs. REM part info
    cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP")
    data$REM.NREM <- NA
    for (k in 1:(length(cycs))){
      if (k<length(NREMPs)){
        if (data$CycleStart[cycs[k]] == "NREMP"){ 
          data$REM.NREM[c((cycs[k]):((cycs[k+1])-1))] <- 0
        }else if (data$CycleStart[cycs[k]] == "REMP"){
          data$REM.NREM[c((cycs[k]):((cycs[k+1])-1))] <- 1
        }
      }else{
        if (data$CycleStart[cycs[k]] == "NREMP"){ 
          data$REM.NREM[c((cycs[k]):(nrow(data)))] <- 0
        }else if (data$CycleStart[cycs[k]] == "REMP"){
          data$REM.NREM[c((cycs[k]):(nrow(data)))] <- 1
        }
      }
    }
    
    # remove incomplete NREM-REM cycle at the end of the night (i.e., cycles followed by <5min NREM or W)
    if(!exists("rm_incompletecycs")){
      stop ("Please specify whether you want to remove or keep the incomplete last cycle.")
    } 
    
    if (rm_incompletecycs == T){
      cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP") #remove NREM/W following last REMP or REM/W following last NREMP
      if (data$CycleStart[cycs[length(cycs)]] == "REMP"){
        REMs <- which(data$Descr3 == "REM") #which 30s epochs are REM
        stop <- REMs[length(REMs)]+1 # stop after last REM epoch
        end <- which(data$Descr3 == "NREM")
        end <- end[end>=stop]
        if(length(end)>10){
          data$CycleStart[stop] <- "stop"
          data$cycles[stop:nrow(data)] <- NA
          data$REM.NREM[stop:nrow(data)] <- NA
        }else{
          # remove last REMP
          stop <- cycs[length(cycs)]
          data$CycleStart[cycs[length(cycs)]] <- "stop"
          data$cycles[stop:nrow(data)] <- NA
          data$REM.NREM[stop:nrow(data)] <- NA
        }
      }else{
        NREMs <- which(data$Descr3 == "NREM") #which 30s epochs are NREM
        stop <- NREMs[length(NREMs)]+1 # stop after last NREM epoch
        end <- which(data$Descr3 == "REM")
        end <- end[end>=stop]
        if(length(end)>10){
          data$CycleStart[stop] <- "stop"
          data$cycles[stop:nrow(data)] <- NA
          data$REM.NREM[stop:nrow(data)] <- NA
        }else{
          # remove last NREMP
          stop <- cycs[length(cycs)]
          data$CycleStart[cycs[length(cycs)]] <- "stop"
          data$cycles[stop:nrow(data)] <- NA
          data$REM.NREM[stop:nrow(data)] <- NA
        }
      }
      lastREMP <- cycs[length(cycs)] # find end of last REMP
      end <- which(data$Descr3 == "NREM" | data$Descr3 == "W")
      end <- end[c(end > lastREMP)] # NREM or wake following onset of last REMP
      stop <- NA
    }else if (rm_incompletecycs == F){
      #remove NREM/W following last REMP or REM/W following last NREMP
      cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP") 
      if (data$CycleStart[cycs[length(cycs)]] == "REMP"){
        REMs <- which(data$Descr3 == "REM") #which 30s epochs are REM
        stop <- REMs[length(REMs)]+1 # stop after last REM epoch
        data$CycleStart[stop] <- "stop"
        data$cycles[stop:nrow(data)] <- NA
        data$REM.NREM[stop:nrow(data)] <- NA
      }else{
        lastNREMP <-  tail(which(data$CycleStart == "NREMP"),1)
        Ws <- which(data$Descr3 == "W") #which 30s epochs are W
        Ws <- Ws[Ws > lastNREMP]
        if (length(Ws) > 2){
          # find last continuous W episode (i.e., final W)
          Ws_start <- NA
          for (kk in 2:(length(Ws))){
            if (Ws[kk-1] != Ws[kk]-1){ 
              Ws_start <- c(Ws_start, Ws[kk])
            }else{
              next
            }
          }
          if (!all(is.na(Ws_start))){
            Ws_start <- tail(Ws_start,1) #last is the onset of the last wake episode
          }else{
            Ws_start <- head(Ws,1)
          }
          data$CycleStart[Ws_start] <- "stop"
          # now check again if >=15min criterion is still fulfilled for the last NREMP
          if(Ws_start - tail(which(data$CycleStart == "NREMP"),1)>=30){
            data$cycles[Ws_start:nrow(data)] <- NA
            data$REM.NREM[Ws_start:nrow(data)] <- NA
          }else{
            # if not, remove last NREMP completely
            data$CycleStart[tail(which(data$CycleStart == "NREMP"),1)] <- "stop"
            data$CycleStart[Ws_start] <- NA
            data$cycles[tail(which(data$CycleStart == "stop"),1):nrow(data)] <- NA
            data$REM.NREM[tail(which(data$CycleStart == "stop"),1):nrow(data)] <- NA
            
            #now again remove NREM/W following last REMP
            cycs <- which(data$CycleStart == "NREMP" | data$CycleStart == "REMP") 
            if (data$CycleStart[cycs[length(cycs)]] == "REMP"){
              REMs <- which(data$Descr3 == "REM") #which 30s epochs are REM
              stop <- REMs[length(REMs)]+1 # stop after last REM epoch
              data$CycleStart[stop] <- "stop"
              data$cycles[stop:nrow(data)] <- NA
              data$REM.NREM[stop:nrow(data)] <- NA
            }
          }
        }
      }
    }
    
    ## merge cycle marker & NREM/REM marker
    data$CycInfo <- paste(data$cycles, data$REM.NREM, sep = "")
    data$CycInfo[data$CycInfo == "NANA"] <- NA
    
    ## add percentiles of NREM & REM parts
    cycs <- na.omit(unique(data$CycInfo))
    data$perc <- NA
    for (k in 1:length(cycs)){
      length <- nrow(subset(data, data$CycInfo == cycs[k]))
      if ((length %% 10) == 0){
        seq <- rep(length/10,10)
        for (kk in 1:length(seq)){
          if (kk == 1){
            ind <- rep(kk, seq[kk])
          }else{
            ind <- c(ind, rep(kk, seq[kk]))
          }
        }
        data$perc[which(data$CycInfo == cycs[k])] <- ind
      }else{
        seq <- sample(c(rep(floor(length/10), (10-length %% 10)), rep(ceiling(length/10), length %% 10))) #construct shuffled sequence of floor/ceiling rounded percentiles that matches the length of the complete cycle
        for (kk in 1:length(seq)){
          if (kk == 1){
            ind <- rep(kk, seq[kk])
          }else{
            ind <- c(ind, rep(kk, seq[kk]))
          }
        }
        data$perc[which(data$CycInfo == cycs[k])] <- ind
      }
    }
    
    ## merge cycle marker, NREM/REM marker, & percentile marker
    data$cycle_perc <- paste(data$CycInfo, data$perc, sep = "_")
    data$cycle_perc[data$cycle_perc == "NA_NA"] <- NA
    
    ## prep new marker file with cycle info
    cycles <- cycles[,-c(4,5)]
    cycles$Description <- data$cycle_perc
    cycles$SleepCycle <- data$cycles
    cycles$N_REM <- data$REM.NREM
    cycles$percentile <- data$perc
    
    ## save new file
    name <- unlist(str_split(filename, pattern = "_"))
    savename <- paste0(c(name[1:(length(name)-1)]), sep = "_", collapse = "")
    savename <- paste(savename, "SCycles.txt", sep = "")
    write.table(cycles, file = savename, row.names = F)
    
    ## plot results if desired
    if (plot == T){
      dfplot <- data
      dfplot <- dfplot[,-c(1,4,5)]
      dfplot$time <- seq(1,nrow(dfplot))
      dfplot$time2 <- (dfplot$Position/(1000/128))/(1000*60)
      dfplot$Description[dfplot$Description == 1] <- -1
      dfplot$Description[dfplot$Description == 2] <- -2
      dfplot$Description[dfplot$Description == 3] <- -3
      dfplot$Description[dfplot$Description == 5] <- 1
      dfplot$CycInfo[dfplot$CycInfo == 10] <- 1.5
      dfplot$CycInfo[dfplot$CycInfo == 11] <- 1.6
      dfplot$CycInfo[dfplot$CycInfo == 20] <- 1.8
      dfplot$CycInfo[dfplot$CycInfo == 21] <- 1.9
      dfplot$CycInfo[dfplot$CycInfo == 30] <- 1.5
      dfplot$CycInfo[dfplot$CycInfo == 31] <- 1.6
      dfplot$CycInfo[dfplot$CycInfo == 40] <- 1.8
      dfplot$CycInfo[dfplot$CycInfo == 41] <- 1.9
      dfplot$CycInfo[dfplot$CycInfo == 50] <- 1.5
      dfplot$CycInfo[dfplot$CycInfo == 51] <- 1.6
      dfplot$CycInfo[dfplot$CycInfo == 60] <- 1.8
      dfplot$CycInfo[dfplot$CycInfo == 61] <- 1.9
      dfplot$CycInfo[dfplot$CycInfo == 70] <- 1.5
      dfplot$CycInfo[dfplot$CycInfo == 71] <- 1.6
      dfplot$CycInfo <- as.numeric(dfplot$CycInfo)
      
      ggplot(dfplot, aes(x=time, y=Description, colour=Description)) + 
        theme_bw()+
        geom_point() +
        geom_line(aes(x=time, y=Description))+
        xlab("Time") +
        ylab("Sleep Stage")+
        # scale_y_continuous(limits = c(-3,5), labels = c("N3", "N2", "N1", "W", "REM", "", "", "", ""))+
        scale_y_continuous(limits = c(-3,2), breaks = c(-3, -2, -1, 0, 1, 2, 3, 4, 5), labels = c("N3", "N2", "N1", "W", "REM", "", "", "", ""))+
        scale_color_viridis(name = "Sleep Stage", option = "D")+
        geom_point(y = dfplot$CycInfo, size = 0.5)
      
      savename <- paste0(c(name[1:(length(name)-2)]), sep = "_", collapse = "")
      savename <- paste(savename, "plot.png", sep = "")
      ggsave(file=savename, width = 25, height = 15, units = "cm", dpi = 600) 
    }

  }
}
